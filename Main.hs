{-# LANGUAGE OverloadedStrings #-}
module Main where

import Antenna.Core
import Control.Applicative
import Control.Arrow                                 ( (***) )
import Control.Lens
import Control.Monad                                 ( liftM, void )
import Database.Persist.Postgresql
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.Wai.Middleware.Cors
import Network.WebSockets                            ( defaultConnectionOptions )
import System.Posix.Env
import System.Posix.Signals              
import Web.Heroku.Postgres

import qualified Data.ByteString.Char8            as C8
import qualified Data.Text                        as Text

corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy = const $ Just $ simpleCorsResourcePolicy
    { corsMethods        = ["OPTIONS", "GET", "POST", "PUT", "PATCH", "DELETE"]
    , corsRequestHeaders = ["Authorization"] }

main :: IO ()
main = do
    port <- read <$> getEnvDefault "PORT" "3333"
    -- herokuParams <- dbConnParams
    -- let opts = (Text.unpack *** Text.unpack) <$> herokuParams
    pool <- inIO $ createPostgresqlPool (connectionStr opts) 10
    let state = AppState pool 
    let settings = defaultSettings 
            & setPort port
            & setInstallShutdownHandler (void . signalHandlers)
    runSettings settings 
        $ cors corsPolicy 
        $ websocketsOr defaultConnectionOptions (wsApp state) (waiApp state)
  where
    signalHandlers onClose = do
        installHandler sigTERM (Catch $ term onClose) (Just fullSignalSet)
        installHandler sigHUP  (Catch $ hup  onClose) (Just fullSignalSet)
    connectionStr opts = C8.pack $ unwords [ key ++ "=" ++ val | (key, val) <- opts ]

hup _ = print "HUP"
term close = print "TERM" >> close

opts :: [(String, String)]
opts = [ ("host"     , "localhost")
       , ("user"     , "antenna")
       , ("password" , "antenna")
       , ("dbname"   , "antenna")
       ]
