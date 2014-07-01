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
import Focus.Types (Focus, ArtistFocus, formArtistFocus)
import Artist.Splices
import Application
import Helpers

focusesResource :: Resource
focusesResource = Resource "focuses" "/admin/focuses" [] []

focusesCrud :: [(CRUD, AppHandler ())]
focusesCrud = [ (RNew, newH)
                     , (RCreate, newH)
                     , (RIndex, indexH)
                     ]


newH :: AppHandler ()
newH = do r <- runForm "new" (formlet Nothing)
          case r of
            (v, Nothing) -> renderWithSplices "focus/new" (digestiveSplices v)
            (_, Just focus) -> do runGH $ insert_ (focus :: Focus)
                                  redirect "/"

indexH :: AppHandler ()
indexH = render "focus/index"


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
