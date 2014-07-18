{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Artist.Splices where

import Data.Maybe
import Control.Monad.Trans (lift)
import Snap.Snaplet.Groundhog.Postgresql
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Utils (selectEntity, Entity(..))
import Database.Groundhog.Utils.Postgresql
import Snap.Restful.TH
import Heist
import Heist.Interpreted

import Application
import Artist.Types
import Subject.Types (Subject)
import qualified Subject.Types as S
import Focus.Types (Focus)
import qualified Focus.Types as F
import Media.Types

artistSplices :: Entity (AutoKey Artist) Artist -> Splices (Splice AppHandler)
artistSplices a =
  do artistSplices' (entityVal a)
     "id" ## textSplice (T.pack $ show $ keyToInt $ entityKey a)
     "media" ## do ms <- lift $ runGH $ selectEntity MediaConstructor (ArtistIdField ==. (keyToInt $ entityKey a))
                   mapSplices (runChildrenWith . mediaSplices) ms
     "subjects" ##
       do as <- lift $ runGH $ selectEntity S.ArtistSubjectConstructor
                                            (S.ArtistIdField ==. (keyToInt $ entityKey a))
          ss <- mapM (\a -> do let k = (intToKey $ S.subjectId $ entityVal a)
                               s <- lift $ runGH $ get k
                               return $ maybe Nothing (Just . Entity (intToKey .
                                                                      S.subjectId .
                                                                      entityVal $ a)) s) as
          mapSplices (runChildrenWith . subjectSplices) (catMaybes ss)
     "focuses" ##
       do as <- lift $ runGH $ selectEntity F.ArtistFocusConstructor
                                            (F.ArtistIdField ==. (keyToInt $ entityKey a))
          ss <- mapM (\a -> do let k = (intToKey $ F.focusId $ entityVal a)
                               s <- lift $ runGH $ get k
                               return $ maybe Nothing (Just . Entity (intToKey .
                                                                      F.focusId .
                                                                      entityVal $ a)) s) as
          mapSplices (runChildrenWith . focusSplices) (catMaybes ss)

artistSplices' :: Artist -> Splices (Splice AppHandler)
artistSplices' = $(iSplices ''Artist)


subjectSplices :: Entity (AutoKey Subject) Subject -> Splices (Splice AppHandler)
subjectSplices a = do subjectSplices' (entityVal a)
                      "id" ## textSplice (T.pack $ show $ keyToInt $ entityKey a)

subjectSplices' :: Subject -> Splices (Splice AppHandler)
subjectSplices' = $(iSplices ''Subject)


focusSplices :: Entity (AutoKey Focus) Focus -> Splices (Splice AppHandler)
focusSplices a = do focusSplices' (entityVal a)
                    "id" ## textSplice (T.pack $ show $ keyToInt $ entityKey a)

focusSplices' :: Focus -> Splices (Splice AppHandler)
focusSplices' = $(iSplices ''Focus)
