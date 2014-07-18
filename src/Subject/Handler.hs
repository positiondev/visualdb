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
import Subject.Types
import Artist.Splices
import Application
import Helpers

subjectsResource :: Resource
subjectsResource = Resource "subjects" "/admin/subjects" [] []

subjectsCrud :: [(CRUD, AppHandler ())]
subjectsCrud = [ (RNew, newH)
               , (RCreate, newH)
               , (RIndex, indexH)
               , (RShow, showH)
               , (RDestroy, destroyH)
               ]


newH :: AppHandler ()
newH = do r <- runForm "new" (formlet Nothing)
          case r of
            (v, Nothing) -> renderWithSplices "subject/new" (digestiveSplices v)
            (_, Just subject) -> do runGH $ insert_ (subject :: Subject)
                                    redirect "/"

indexH :: AppHandler ()
indexH = render "subject/index"

destroyH :: AppHandler ()
destroyH = do i <- getId
              let k = intToKey i :: AutoKey Subject
              runGH $ deleteBy k
              redirectReferer


showH :: AppHandler ()
showH =
  do i <- getId
     let k = intToKey i :: AutoKey Subject
     me <- fmap (Entity k) <$> runGH (get k)
     case me of
       Nothing -> pass
       Just e -> do
         as <- runGH $ selectEntity ArtistSubjectConstructor (SubjectIdField ==. i)
         artists <- mapM (\a -> do let k = intToKey (artistId (entityVal a))
                                   r <- runGH $ get k
                                   return $ fmap (Entity k) r) as
         renderWithSplices "subject/show"
                           (do "artists" ## mapSplices (runChildrenWith.artistSplices) (catMaybes artists)
                               "count" ## textSplice (T.pack (show (length artists)))
                               subjectSplices e)

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
