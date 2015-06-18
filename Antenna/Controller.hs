{-# LANGUAGE OverloadedStrings #-}
module Antenna.Controller where

import Antenna.Db
import Antenna.Db.Schema
import Antenna.Types
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Middleware.HttpAuth
import Web.Frank
import Web.Simple

authenticate :: (Node -> AppController ()) -> AppController ()
authenticate action = do
    req <- request
    --credentials <- credentials req
    action undefined

credentials :: Request -> MaybeT IO Node
credentials request = do
    return $ do
        headers <- lookup "Authorization" (requestHeaders request)
        (user, pass) <- extractBasicAuth headers
        return undefined
    undefined
--    node <- decodeUtf8 user `MapS.lookup` (state ^. nodes)
--    case node of
--        DeviceNode secret | secret == decodeUtf8 pass -> Just node
--        _ -> Nothing

controller :: AppController ()
controller = do

    get "ping" $ 
        respond (responseLBS status200 [] "Pong!")

    get "stack" $ authenticate $ 
        \node -> do
            state <- controllerState
            nodes <- liftIO $ runDb (state ^. sqlPool) getNodes
            respond (responseLBS status200 [] "...")

