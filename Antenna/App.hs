{-# LANGUAGE OverloadedStrings #-}
module Antenna.App 
    ( appSetup
    , waiApp
    ) where

import Antenna.Controller
import Antenna.Db
import Antenna.Db.Schema
import Antenna.Types
import Control.Applicative
import Control.Lens
import Control.Monad                                 ( liftM, void )
import Crypto.PasswordStore
import Database.Persist.Postgresql
import Network.Wai.Handler.Warp
import System.Posix.Env
import System.Posix.Signals              
import Web.Simple

import qualified Data.ByteString.Char8            as C8

waiApp :: AppState -> Application
waiApp state = controllerApp state controller 

appSetup :: IO (AppState, Settings)
appSetup = do
    port <- read <$> getEnvDefault "PORT" "3333"
    -- herokuParams <- dbConnParams
    -- let opts = (Text.unpack *** Text.unpack) <$> herokuParams
    pool <- inIO $ createPostgresqlPool (connectionStr opts) 10
    runDb pool $ runMigration migrateAll
    let state = AppState pool (makeSalt "Mxg4YN0OaE3xaehmg3up")
    let settings = defaultSettings & setPort port
                                   & setInstallShutdownHandler (void . signalHandlers)
    return (state, settings)
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
       , ("dbname"   , "antenna_tests") ]

