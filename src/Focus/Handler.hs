{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Focus.Handler  where

import Data.Maybe
import Data.Monoid
import Snap hiding (get)
import Snap.Restful
import Text.Digestive.Snap
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Database.Groundhog
import Snap.Snaplet.Groundhog.Postgresql
import Database.Groundhog.Utils hiding (intToKey)
import Database.Groundhog.Utils.Postgresql
import Snap.Snaplet.Heist
import Snap.Extras
import Text.Digestive.Heist
import Heist
import Heist.Interpreted

import Artist.Types
import Artist.Handler (artistsResource)
import Focus.Types
import Artist.Splices
import Application
import Helpers

focusesResource :: Resource
focusesResource = Resource "focuses" "/admin/focuses" [] []

focusesCrud :: [(CRUD, AppHandler ())]
focusesCrud = [ (RNew, newH)
              , (RCreate, newH)
              , (RIndex, indexH)
              , (RShow, showH)
              , (RDestroy, destroyH)
              ]


newH :: AppHandler ()
newH = do r <- runForm "new" (formlet Nothing)
          case r of
            (v, Nothing) -> renderWithSplices "focus/new" (digestiveSplices v)
            (_, Just focus) -> do runGH $ insert_ (focus :: Focus)
                                  redirect "/"

indexH :: AppHandler ()
indexH = render "focus/index"

destroyH :: AppHandler ()
destroyH = do i <- getId
              let k = intToKey i :: AutoKey Focus
              runGH $ deleteBy k
              redirectReferer

showH :: AppHandler ()
showH =
  do i <- getId
     let k = intToKey i :: AutoKey Focus
     me <- fmap (Entity k) <$> runGH (get k)
     case me of
       Nothing -> pass
       Just e -> do
         as <- runGH $ selectEntity ArtistFocusConstructor (FocusIdField ==. i)
         artists <- mapM (\a -> do let k = intToKey (artistId (entityVal a))
                                   r <- runGH $ get k
                                   return $ fmap (Entity k) r) as
         renderWithSplices "focus/show"
                           (do "artists" ## mapSplices (runChildrenWith.artistSplices) (catMaybes artists)
                               "count" ## textSplice (T.pack (show (length artists)))
                               focusSplices e)

artistFocusesResource :: Resource
artistFocusesResource = Resource "artistFocuses" "/admin/artistfocuses" [] []

artistFocusesCrud :: [(CRUD, AppHandler ())]
artistFocusesCrud = [ (RNew, snewH)
                    , (RCreate, snewH)
                    , (RDestroy, sdestroyH)
                    ]

snewH :: AppHandler ()
snewH = do r <- runForm "new" formArtistFocus
           aid <- T.decodeUtf8 . fromJust <$> getParam "artist_id"
           case r of
             (v, Nothing) -> renderWithSplices "artistfocus/new"
                                               (do digestiveSplices v
                                                   "artist_id" ## textSplice aid)
             (_, Just focus) -> do runGH $ insert_ (focus :: ArtistFocus)
                                   redirect $ T.encodeUtf8 $ T.concat [rootPath artistsResource, "/", aid]


sdestroyH :: AppHandler ()
sdestroyH = do i <- getId
               let k = intToKey i :: AutoKey ArtistFocus
               runGH $ deleteBy k
               redirectReferer
