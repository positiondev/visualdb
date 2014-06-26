{-# LANGUAGE OverloadedStrings, GADTs, TypeFamilies, TemplateHaskell,
             QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}

module Artist.Types where

import Control.Monad (join)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.Groundhog.TH
import Database.Groundhog.Postgresql
import Database.Groundhog.Utils hiding (keyToInt)
import Database.Groundhog.Utils.Postgresql
import Snap.Restful
import Snap.Restful.TH
import Text.Digestive.Form
import Heist
import Heist.Interpreted

import Application

instance HasFormlet (Maybe Int) where
  formlet = optionalStringRead "Could not read number." . join

data ArtistType = Illustrator | Photographer deriving (Eq, Show)

focusList :: ArtistType -> [Text]
focusList Illustrator = ["Animation", "Editorial", "Covers", "Icons"]
focusList Photographer = ["Documentary", "Portrait", "Black and White", "Landscape"]

data Subject = Subject { title :: Text
                       } deriving (Eq, Show)


data Artist = Artist { name :: Text
                     , website :: Text
                     , typ :: Text
                     , email :: Text
                     , agency :: Text
                     , city :: Text
                     , country :: Text
                     , focus :: Text
                     , subjectId :: Maybe Int
                     , notes :: Text
                     }
mkPersist defaultCodegenConfig { namingStyle = lowerCaseSuffixNamingStyle } [groundhog|
- entity: Subject
- entity: Artist
|]

deriveHasFormlet ''Subject
deriveHasFormlet ''Artist


artistSplices :: Entity (AutoKey Artist) Artist -> Splices (Splice AppHandler)
artistSplices a = do artistSplices' (entityVal a)
                     "id" ## textSplice (T.pack $ show $ keyToInt $ entityKey a)

artistSplices' :: Artist -> Splices (Splice AppHandler)
artistSplices' = $(iSplices ''Artist)
