{-# LANGUAGE OverloadedStrings #-}
module Main where

import Antenna.Core
import Antenna.Db
import Antenna.Db.Schema
import Antenna.Tests
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.Wai.Middleware.Cors
import Network.WebSockets                            ( defaultConnectionOptions )

corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy = const $ Just $ simpleCorsResourcePolicy
    { corsMethods        = ["OPTIONS", "GET", "POST", "PUT", "PATCH", "DELETE"]
    , corsRequestHeaders = ["Authorization"] }

main :: IO ()
main = do
--    runTests 
    (state, settings) <- appSetup
    ----------------------------------------------------
    createRootUser (_sqlPool state) (_salt state)
    ----------------------------------------------------
    runSettings settings 
        $ cors corsPolicy 
        $ websocketsOr defaultConnectionOptions (wsApp state) (waiApp state)

createRootUser pool salt = do
    runDb pool $ insertNode $ NewNode "root" Device (Just $ makePwd "root" salt) True
    return ()

