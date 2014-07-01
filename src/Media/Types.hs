{-# LANGUAGE OverloadedStrings, GADTs, TypeFamilies, TemplateHaskell,
             QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}

module Media.Types where

import Control.Lens
import Control.Monad (join)
import Control.Monad.Trans (lift)
import Control.Applicative
import Data.Text (Text)
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.Groundhog.TH
import Database.Groundhog.Postgresql
import Database.Groundhog.Utils hiding (keyToInt)
import Database.Groundhog.Utils.Postgresql
import Snap
import Snap.Restful
import Snap.Restful.TH
import Text.Digestive
import Text.Digestive.Form
import Heist
import Heist.Interpreted

import FileStore
import Application


data Media = Media { artistId :: Int
                   , url :: Text
                   }
mkPersist defaultCodegenConfig { namingStyle = lowerCaseSuffixNamingStyle } [groundhog|
- entity: Media
|]

formletMedia v = validateM mkMedia $
                     (,) <$> "file" .: file
                         <*> "artist_id" .: stringRead "Must be a number." (artistId <$> v)
    where mkMedia :: (Maybe FilePath, Int) -> AppHandler (Result Text Media)
          mkMedia (Nothing, aid) = return $ Success $ Media aid (fromMaybe "" (url <$> v))
          mkMedia (Just path, aid) =  do store <- use filestore
                                         url <- storeFile store path Nothing
                                         return $ Success $ Media aid url

mediaSplices :: Entity (AutoKey Media) Media -> Splices (Splice AppHandler)
mediaSplices a = do mediaSplices' (entityVal a)
                    "id" ## textSplice (T.pack $ show $ keyToInt $ entityKey a)

mediaSplices' :: Media -> Splices (Splice AppHandler)
mediaSplices' = $(iSplices ''Media)
