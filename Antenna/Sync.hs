{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Antenna.Sync where

import Antenna.Db                                    
import Antenna.Types
import Control.Applicative
import Control.Lens
import Control.Monad                                 ( unless, when, forM_ )
import Control.Monad.Trans                           ( liftIO )
import Data.Aeson
import Data.Function                                 ( on )
import Data.List                                     ( intersect, (\\), sortBy )
import Data.Monoid
import Data.Text                                     ( Text, splitOn, isInfixOf )
import Data.Text.Encoding                            ( encodeUtf8, decodeUtf8 )
import Database.Esqueleto                            ( Key, unValue, entityKey, entityVal )
import Database.Persist                              ( insertMany )
import Network.AMQP                                  ( DeliveryMode(..), Message(..), newMsg, publishMsg )
import Network.HTTP.Types
import Web.Simple

import qualified Antenna.Db.Schema                as Db
import qualified Data.ByteString.Lazy             as BL
import qualified Data.HashMap.Strict              as MapS
import qualified Data.Text                        as Text
import qualified Text.Show.Text                   as Text

processSyncRequest :: Node -> SyncRequest -> AppController ()
processSyncRequest node SyncRequest{..} = do
    state <- controllerState 
    response <- liftIO $ runDb (state ^. sqlPool) $ do

        -- Update sync points for all nodes to the least recent (min) of the current
        -- value and the timestamp of the first item in the commit log
        unless (null reqSyncLog) $ do
            updated <- Db.updateTimestamp (takeMin reqSyncLog) 
            liftIO $ print updated
            -- Broadcast websocket notifications
            forM_ updated $ \_node -> when (_node /= node ^. name) $ do
                liftIO $ publishMsg (state ^. channel) "antenna" "" $ newMsg 
                    { msgBody = BL.fromStrict (encodeUtf8 _node)
                    , msgDeliveryMode = Just Persistent }

        let sourceKey = node ^. nodeId & Db.toKey

        nodeSyncPoint <- Db.getNodeSyncPoint sourceKey

        let (tstamp, isAhead) = 
                if reqSyncPoint < nodeSyncPoint
                    then (reqSyncPoint  , True)
                    else (nodeSyncPoint , False)

        reverseActions <- Db.getReverseActions sourceKey tstamp

        commitId <- Db.getMaxCommitId

        -- Insert commited transactions and annote transactions with the commit id 
        transactionIds <- insertMany $ translate sourceKey (succ commitId) <$> reqSyncLog

        candidates <- Db.selectNodeCollection reqSyncTargets (Db.toKey <$> node ^. targets)
        let candidateTargets = entityKey <$> candidates

        -- Collect transactions for which the range includes the source node or a candidate target 
        let targets = cons sourceKey candidateTargets

        Db.addToTransactionRange_ transactionIds sourceKey

        forwardActions <- Db.getForwardActions targets tstamp

        let keys = (Db.toKey . _transactionId <$> forwardActions) \\ transactionIds
        Db.addToTransactionRange_ keys sourceKey

        -- Virtual nodes
        let virtualNodes = filter (\t -> "virtual" == Db.nodeFamily (entityVal t)) candidates

        forM_ virtualNodes $ \_node -> 
            Db.addToTransactionRange_ transactionIds (entityKey _node)
 
        -- Update sync point for source node
        sp <- Db.setNodeSyncPoint sourceKey
    
        return SyncResponse 
                { respRewind    = 
                    if isAhead then []
                               else _downAction <$> reverseActions 
                , respForward   = _upAction <$> forwardActions 
                , respSyncPoint = sp
                }

    respondWith status200 (toJSON response)

translate :: Key Db.Node -> Int -> Transaction -> Db.Transaction
translate nodeId commitId Transaction{..} = 
    Db.Transaction
        nodeId
        commitId
        _batchIndex
        -- Up action
        (_upAction   ^. method   & toJSON & showMethod)
        (_upAction   ^. resource & replace_ commitId)
        (_upAction   ^. payload  & encoded)
        -- Down action
        (_downAction ^. method   & toJSON & showMethod)
        (_downAction ^. resource & replace_ commitId)
        (_downAction ^. payload  & encoded)
        -- Timestamp
        (fromIntegral ts)
  where
    Timestamp ts = _timestamp
    showMethod (String mtd) = mtd
    showMethod ____________ = ""
    encoded Nothing = ""
    encoded (Just cmd) = decodeUtf8 $ BL.toStrict $ encode $ updObj commitId cmd

takeMin :: [Transaction] -> Int
takeMin ts = fromIntegral t
  where
    (Timestamp t) = minimum $ map _timestamp ts

updObj :: Int -> Value -> Value
updObj commitId (Object o) = Object (MapS.mapWithKey deep o)
  where
    deep "href" (String val) = String (replace_ commitId val)
    deep _ o = updObj commitId o
updObj _ o = o

replace_ :: Text.Show a => a -> Text -> Text
replace_ commitId txt = splitOn "||" txt & zipWith (curry go) [1 .. ] & Text.concat 
  where
    go (i, p) | odd i     = p
              | otherwise = 
                 case splitOn "/" p of
                   [_, i] | "-" `isInfixOf` i -> p
                   [r, i] -> r <> "/id_" <> Text.show commitId <> "-" <> i 
                   ______ -> p

