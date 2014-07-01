{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Artist.Handler (artistsResource, artistsCrud) where

import Data.Maybe
import Data.Monoid
import Snap hiding (get)
import Snap.Restful
import Text.Digestive.Snap
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T
import Database.Groundhog
import Snap.Snaplet.Groundhog.Postgresql
import Database.Groundhog.Utils hiding (intToKey)
import Database.Groundhog.Utils.Postgresql
import Snap.Snaplet.Heist
import Text.Digestive.Heist
import Heist.Interpreted

import Artist.Types
import Subject.Types (Subject)
import Artist.Splices
import Application
import Helpers

artistsResource :: Resource
artistsResource = Resource "artists" "/admin/artists" [] []

artistsCrud :: [(CRUD, Handler App App ())]
artistsCrud = [ (RNew, newH)
              , (RShow, showH)
              , (REdit, editH)
              , (RUpdate, editH)
              , (RCreate, newH)
              , (RIndex, indexH)
              ]



newH :: AppHandler ()
newH = do r <- runForm "new" (formletArtist Nothing)
          case r of
            (v, Nothing) -> renderWithSplices "artist/new" (digestiveSplices v)
            (_, Just artist) -> do runGH $ insert_ (artist :: Artist)
                                   redirect "/"

showH :: AppHandler ()
showH = do i <- getId
           let k = intToKey i
           me <- runGH $ get k
           case me of
             Nothing -> pass
             Just e -> renderWithSplices "artist/show" (artistSplices (Entity k e))

editH :: AppHandler ()
editH =
  do i <- getId
     let k = intToKey i
     me <- runGH $ get k
     case me of
       Nothing -> pass
       Just e ->
         do r <- runForm "edit" (formletArtist (Just e))
            case r of
              (v, Nothing) -> renderWithSplices "artist/edit"
                                                (digestiveSplices v <> artistSplices (Entity k e))
              (_, Just artist) ->
                do runGH $ replace k (artist :: Artist)
                   redirect $ B.concat [T.encodeUtf8 $ rootPath artistsResource,
                                       "/", B8.pack $ show i]


indexH :: AppHandler ()
indexH = undefined
