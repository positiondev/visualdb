{-# LANGUAGE OverloadedStrings, GADTs, TypeFamilies, TemplateHaskell,
             QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}

module Subject.Types where

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

data Subject = Subject { title :: Text } deriving (Eq, Show)

data ArtistSubject = ArtistSubject { artistId :: Int, subjectId :: Int } deriving (Eq, Show)

mkPersist defaultCodegenConfig { namingStyle = lowerCaseSuffixNamingStyle } [groundhog|
- entity: Subject
- entity: ArtistSubject
|]

deriveHasFormlet ''Subject


formArtistSubject :: Form Text AppHandler ArtistSubject
formArtistSubject =
  ArtistSubject <$> "artistId" .: stringRead "Must be an integer." Nothing
                <*> "subjectId" .: (monadic $ do ss <- runGH selectAll
                                                 return $ choice (map (\(k,s) -> (keyToInt k, title s)) ss) Nothing)
