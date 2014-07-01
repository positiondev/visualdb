{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Subject.Handler  where

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
import Subject.Types (Subject, ArtistSubject, formArtistSubject)
import Artist.Splices
import Application
import Helpers

subjectsResource :: Resource
subjectsResource = Resource "subjects" "/admin/subjects" [] []

subjectsCrud :: [(CRUD, AppHandler ())]
subjectsCrud = [ (RNew, newH)
               , (RCreate, newH)
               , (RIndex, indexH)
               ]


newH :: AppHandler ()
newH = do r <- runForm "new" (formlet Nothing)
          case r of
            (v, Nothing) -> renderWithSplices "subject/new" (digestiveSplices v)
            (_, Just subject) -> do runGH $ insert_ (subject :: Subject)
                                    redirect "/"

indexH :: AppHandler ()
indexH = render "subject/index"


artistSubjectsResource :: Resource
artistSubjectsResource = Resource "artistSubjects" "/admin/artistsubjects" [] []

artistSubjectsCrud :: [(CRUD, AppHandler ())]
artistSubjectsCrud = [ (RNew, snewH)
                     , (RCreate, snewH)
                     , (RDestroy, sdestroyH)
                     ]

snewH :: AppHandler ()
snewH = do r <- runForm "new" formArtistSubject
           aid <- T.decodeUtf8 . fromJust <$> getParam "artist_id"
           case r of
             (v, Nothing) -> renderWithSplices "artistsubject/new"
                                               (do digestiveSplices v
                                                   "artist_id" ## textSplice aid)
             (_, Just subject) -> do runGH $ insert_ (subject :: ArtistSubject)
                                     redirect $ T.encodeUtf8 $ T.concat [rootPath artistsResource, "/", aid]


sdestroyH :: AppHandler ()
sdestroyH = do i <- getId
               let k = intToKey i :: AutoKey ArtistSubject
               runGH $ deleteBy k
               redirectReferer
