{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Artist.Handler (artistsResource, artistsCrud) where

import Data.Maybe
import Snap hiding (get)
import Snap.Restful
import Text.Digestive.Snap
import qualified Data.ByteString.Char8 as B8
import Database.Groundhog
import Snap.Snaplet.Groundhog.Postgresql
import Database.Groundhog.Utils hiding (intToKey)
import Database.Groundhog.Utils.Postgresql
import Snap.Snaplet.Heist
import Text.Digestive.Heist

import Artist.Types
import Application

artistsResource :: Resource
artistsResource = Resource "artists" "/admin/artists" [] []

artistsCrud :: [(CRUD, Handler App App ())]
artistsCrud = [ (RNew, newCreateH)
              , (RShow, showH)
              , (REdit, editH)
              , (RUpdate, updateH)
              , (RCreate, newCreateH)
              , (RIndex, indexH)
              ]


readSafe :: Read a => String -> Maybe a
readSafe = fmap fst . listToMaybe . reads

getId :: AppHandler Int
getId = do mi <- getParam "id"
           case readSafe . B8.unpack =<<  mi  of
             Nothing -> pass
             Just i -> return i

newCreateH :: AppHandler ()
newCreateH = do r <- runForm "new" (formlet Nothing)
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
editH = undefined

updateH :: AppHandler ()
updateH = undefined


indexH :: AppHandler ()
indexH = undefined
