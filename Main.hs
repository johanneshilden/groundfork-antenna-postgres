{-# LANGUAGE OverloadedStrings #-}
module Main where

import Antenna.Core
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
    -- runTests 
    (state, settings) <- appSetup
    runSettings settings 
        $ cors corsPolicy 
        $ websocketsOr defaultConnectionOptions (wsApp state) (waiApp state)

