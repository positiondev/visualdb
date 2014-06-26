{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

import           Control.Monad.Trans
import           Control.Applicative
import           Data.Monoid
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
import           Heist.Splices.BindStrict
import qualified Text.XmlHtml as X

import           Database.Groundhog.Utils
import           Snap.Snaplet.Groundhog.Postgresql
import           Snap.Restful

import           Artist.Types
import           Artist.Handler
import           Application

logoutH :: Handler App (AuthManager App) ()
logoutH = logout >> redirect "/"


loginH :: AppHandler ()
loginH = (method GET  $ renderWithSplices "login" ("err" ## return [])) <|>
         (method POST $ with auth $
            loginUser "login" "password" Nothing
                      (const (renderWithSplices "login" $
                                do "err" ## I.runChildrenWithText ("msg" ## msg)))
                      (redirect "/"))
  where msg = "Unknown user or password"

signupH :: AppHandler ()
signupH = (method GET  $ render "signup") <|>
          (method POST $ with auth $
            registerUser "login" "password" >> redirect "/")

routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    loginH)
         , ("/logout",   with auth logoutH)
         , ("/new_user", signupH)
         , ("",          serveDirectory "static")
         ]

globalSplices =
  do "allArtists" ## do artists <- lift $ runGH $ selectAll
                        I.mapSplices (I.runChildrenWith . artistSplices . uncurry Entity) artists
     "requireLogin" ## do isLI <- lift $ with auth isLoggedIn
                          case isLI of
                            True -> return []
                            False -> redirect "/login"
     "bindStrict" ## bindStrictImpl

app :: SnapletInit App App
app = makeSnaplet "app" "" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"

    gh <- nestSnaplet "gh" gh initGroundhogPostgres
    addRoutes routes
    addResource artistsResource artistsCrud [] [] h
    addConfig h mempty { hcInterpretedSplices = globalSplices }
    addAuthSplices h auth
    return $ App h s a gh
