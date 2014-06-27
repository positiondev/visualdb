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
import Snap.Extras
import Heist
import Heist.Interpreted

import Media.Types
import Application
import Helpers

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
            , (RDestroy, destroyH)
            ]


newH :: AppHandler ()
newH = do r <- runForm' "new" (formletMedia Nothing)
          case r of
            (v, Nothing) ->
              do aid <- getParam "artist_id"
                 ref <- getParam "referer"
                 renderWithSplices "media/new"
                                   (do digestiveSplices v
                                       "artist_id" ## textSplice (T.decodeUtf8 $ fromMaybe "" aid)
                                       "referer" ## textSplice (T.decodeUtf8 $ fromMaybe "" ref))

            (_, Just media) -> do runGH $ insert_ (media :: Media)
                                  ref <- getParam "referer"
                                  redirect (fromMaybe (T.encodeUtf8 $ rootPath mediaResource) ref)

showH :: AppHandler ()
showH = undefined

editH :: AppHandler ()
editH = undefined

indexH :: AppHandler ()
indexH = render "media/index"


destroyH :: AppHandler ()
destroyH = do i <- getId
              let k = intToKey i :: AutoKey Media
              runGH $ deleteBy k
              redirectReferer
