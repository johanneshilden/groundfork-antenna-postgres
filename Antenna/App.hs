{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Antenna.App 
    ( appSetup
    , waiApp
    ) where

import Antenna.Controller
import Antenna.Db
import Antenna.Db.Schema
import Antenna.Types
import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad                                 ( liftM, void )
import Crypto.PasswordStore
import Database.Persist.Postgresql
import Network.AMQP
import Network.Wai.Handler.Warp
import System.Posix.Env
import System.Posix.Signals              
import Web.Simple
import Web.Hashids
import Web.Heroku
import Web.Heroku.RabbitMQ

import qualified Data.ByteString.Char8            as C8
import qualified Data.Text                        as Text

waiApp :: AppState -> Application
waiApp state = controllerApp state controller 

appSetup :: IO (AppState, Settings)
appSetup = do
--    herokuParams <- dbConnParams
--    let opts = (Text.unpack *** Text.unpack) <$> herokuParams

    port <- read <$> getEnvDefault "PORT" "3333"

    pool <- inIO $ createPostgresqlPool (connectionStr opts) 10
    runDb pool $ runMigration migrateAll

--    AmqpSettings{..} <- amqpConnSettings
--    amqp <- openConnection' amqpHostName (fromIntegral amqpPort) amqpVirtualHost amqpUser amqpPass
    amqp <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel amqp 

    declareQueue chan newQueue { queueName = "default" }
    declareExchange chan newExchange { exchangeName = "antenna", exchangeType = "fanout" }
    bindQueue chan "default" "antenna" ""

    let salt     = "Mxg4YN0OaE3xaehmg3up"
        state    = AppState pool (makeSalt salt) chan (hashidsSimple salt)
        settings = defaultSettings & setPort port
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

