{-# LANGUAGE OverloadedStrings, GADTs, TypeFamilies, TemplateHaskell,
             QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}

module Focus.Types where

import Control.Applicative
import Control.Monad (join)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.Groundhog.TH
import Database.Groundhog.Postgresql
import Database.Groundhog.Utils hiding (keyToInt)
import Database.Groundhog.Utils.Postgresql
import Snap.Snaplet.Groundhog.Postgresql
import Snap.Restful
import Snap.Restful.TH
import Text.Digestive.Form
import Heist
import Heist.Interpreted

import Application

data Focus = Focus { title :: Text } deriving (Eq, Show)

data ArtistFocus = ArtistFocus { artistId :: Int, focusId :: Int } deriving (Eq, Show)

mkPersist defaultCodegenConfig { namingStyle = lowerCaseSuffixNamingStyle } [groundhog|
- entity: Focus
- entity: ArtistFocus
|]


deriveHasFormlet ''Focus


formArtistFocus :: Form Text AppHandler ArtistFocus
formArtistFocus =
  ArtistFocus <$> "artistId" .: stringRead "Must be an integer." Nothing
              <*> "focusId" .: (monadic $ do fs <- runGH selectAll
                                             return $ choice (map (\(k,f) -> (keyToInt k, title f)) fs) Nothing)
