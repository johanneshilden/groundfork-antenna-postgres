module Antenna.Socket 
    ( wsApp 
    ) where

import Antenna.App
import Antenna.Types
import Control.Concurrent.STM
import Control.Exception
import Data.ByteString.Lazy                          ( toStrict )
import Data.Text                                     ( Text )
import Data.Text.Encoding                            ( decodeUtf8 )
import Network.WebSockets

listen :: TVar AppState -> Connection -> IO ()
listen tvar connection = loop
  where
    modifyState = atomically . modifyTVar tvar
    loop = do
        message <- receiveDataMessage connection
        case message of
          Text text -> do
            let node = decodeUtf8 (toStrict text)
            modifyState (insertListener node connection)
            loop `catch` close node
          _ -> loop

    close :: Text -> ConnectionException -> IO ()
    close node _ = modifyState (removeListener node)

wsApp :: TVar AppState -> PendingConnection -> IO ()
wsApp tvar pending = do
    connection <- acceptRequest pending
    forkPingThread connection 5
    listen tvar connection 

