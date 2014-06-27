{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Media.Handler (mediaResource, mediaCrud) where

import Data.Maybe
import Data.Monoid
import Snap hiding (get)
import Snap.Restful
import Text.Digestive.Snap
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T
import Database.Groundhog
import Snap.Snaplet.Groundhog.Postgresql
import Database.Groundhog.Utils hiding (intToKey)
import Database.Groundhog.Utils.Postgresql
import Snap.Snaplet.Heist
import Text.Digestive
import Text.Digestive.Heist
import Snap.Util.FileUploads

import Media.Types
import Application

runForm' :: MonadSnap m	=> Text -> Form v m a -> m (View v, Maybe a)
runForm' = runFormWith (defaultSnapFormConfig { uploadPolicy = setMaximumFormInputSize tenmegs defaultUploadPolicy
                                              , partPolicy = const $ allowWithMaximumSize tenmegs})
  where tenmegs = 10 * 1024 * 1024

mediaResource :: Resource
mediaResource = Resource "media" "/admin/media" [] []

mediaCrud :: [(CRUD, Handler App App ())]
mediaCrud = [ (RNew, newH)
            , (RShow, showH)
            , (REdit, editH)
            , (RUpdate, editH)
            , (RCreate, newH)
            , (RIndex, indexH)
            ]


newH :: AppHandler ()
newH = do r <- runForm' "new" (formletMedia Nothing)
          case r of
            (v, Nothing) -> renderWithSplices "media/new" (digestiveSplices v)
            (_, Just media) -> do runGH $ insert_ (media :: Media)
                                  redirect (T.encodeUtf8 $ rootPath mediaResource)

showH :: AppHandler ()
showH = undefined

editH :: AppHandler ()
editH = undefined

indexH :: AppHandler ()
indexH = render "media/index"
