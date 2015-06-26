{-# LANGUAGE OverloadedStrings #-}
module Antenna.Socket where

import Antenna.Types
import Control.Exception
import Control.Lens                                  ( (^.) )
import Data.ByteString.Lazy                          ( toStrict )
import Data.Text                                     ( Text )
import Data.Text.Encoding                            ( decodeUtf8 )
import Network.AMQP                                  ( Channel, Ack(..), DeliveryMode(..), Message(..), newMsg, publishMsg, consumeMsgs )
import Network.WebSockets

listen :: Channel -> AppState -> Connection -> IO ()
listen chan state connection = loop
  where
    loop = do
        receiveDataMessage connection
        loop

--        message <- receiveDataMessage connection
--        case message of
--          Text text -> do
--                publishMsg chan "antenna" "" $ newMsg 
--                    { msgBody = text
--                    , msgDeliveryMode = Just Persistent }
--                loop 
--          _ -> loop

wsApp :: AppState -> PendingConnection -> IO ()
wsApp state pending = do
    connection <- acceptRequest pending
    forkPingThread connection 5
    let chan = state ^. channel
    tag <- consumeMsgs chan "default" Ack $ 
        \(msg, _) -> 
            sendTextData connection (msgBody msg)
    listen chan state connection 

