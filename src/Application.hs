{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------

import           Control.Lens
import           Snap
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Groundhog.Postgresql hiding (get)
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session

import           FileStore

------------------------------------------------------------------------------
data App = App
    { _heist     :: Snaplet (Heist App)
    , _sess      :: Snaplet SessionManager
    , _auth      :: Snaplet (AuthManager App)
    , _gh        :: Snaplet GroundhogPostgres
    , _filestore :: FileStore
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasGroundhogPostgres (Handler b App) where
    getGroundhogPostgresState = use (gh . snapletValue)

------------------------------------------------------------------------------
type AppHandler = Handler App App
