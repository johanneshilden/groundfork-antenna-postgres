{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
module Antenna.Types where

import Control.Applicative
import Control.Lens                                  ( makeLenses, (&) )
import Control.Monad                                 ( mzero )
import Crypto.PasswordStore
import Data.Aeson
import Data.Int                                      ( Int64 )
import Data.Maybe                                    ( maybeToList )
import Data.Scientific
import Data.Text                                     ( Text )
import Data.Typeable
import Database.Persist.Sql
import Web.Simple

import Network.HTTP.Types                     hiding ( Method, GET, PUT, POST, DELETE, PATCH )

data NodeType 
  = Device 
    -- ^ An authenticated device node.
  | Virtual
    -- ^ Nodes that do not represent any actual host device. Unlike ordinary 
    --   nodes, these nodes are automatically forwarded during a sync, if
    --   they appear as a target node. This makes them suitable as simple 
    --   exchange points.
    deriving (Show, Read, Eq)

toText :: NodeType -> Text
toText Virtual = "virtual"
toText _______ = "device"

toType :: Text -> NodeType
toType "virtual" = Virtual
toType _________ = Device

-- | Supported methods
data Method = POST | PUT | PATCH | DELETE
    deriving (Eq, Show)

instance FromJSON Method where
    parseJSON (String "POST")   = return POST
    parseJSON (String "PUT")    = return PUT
    parseJSON (String "PATCH")  = return PATCH
    parseJSON (String "DELETE") = return DELETE
    parseJSON _ = mzero

instance ToJSON Method where
    toJSON POST   = String "POST"
    toJSON PUT    = String "PUT"
    toJSON PATCH  = String "PATCH"
    toJSON DELETE = String "DELETE"

newtype Timestamp = Timestamp Int64
    deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON)

data SyncPoint = AtTime Timestamp | Saturated
    deriving (Eq, Show, Typeable)

toTime :: Scientific -> SyncPoint
toTime = AtTime . Timestamp . fromIntegral . coefficient

instance FromJSON SyncPoint where
    parseJSON (String "*") = return Saturated
    parseJSON (Number   n) = return $ toTime n
    parseJSON _            = mzero

instance ToJSON SyncPoint where
    toJSON Saturated  = String "*"
    toJSON (AtTime t) = toJSON t

-- | A command operates on some application resource using the given method
--   and an optional request object. 
data Command = Command
    { _method   :: Method
    , _resource :: Text
    , _payload  :: Maybe Value
    } deriving (Eq, Show, Typeable)

$(makeLenses ''Command)

instance FromJSON Command where
    parseJSON (Object v) =
        Command <$> v .:  "method"
                <*> v .:  "resource"
                <*> v .:? "payload"
    parseJSON _ = mzero

instance ToJSON Command where
    toJSON Command{..} = object $
        [ "method"   .= _method
        , "resource" .= _resource
        ] ++ case "payload" .= _payload of
              (_,Null) -> []
              pload -> [pload]

data Node = Node
    { _nodeId    :: Int
    , _name      :: Text
    , _family    :: NodeType
    , _targets   :: [Text]
    , _syncPoint :: Int
    , _locked    :: Bool
    } deriving (Show)

data Transaction = Transaction
    { _transactionId :: Int
    -- ^ Primary key
    , _sourceNodeId  :: Int
    -- ^ The source node form which the action originates.
    , _commitId      :: Int
    -- ^ The commit batch to which the action belongs.
    , _batchIndex    :: Int
    -- ^ A batch-specific sequential id assigned to each action.
    , _upAction      :: Command
    -- ^ Command object which encapsulates the forward (redo) action.
    , _downAction    :: Command
    -- ^ Command object which encapsulates the reverse (undo) action.
    , _timestamp     :: Timestamp
    -- ^ A timestamp denoting when the action was created.
    , _range         :: [Text]
    -- ^ A set containing the nodes that have executed this action.
    } deriving (Show)

$(makeLenses ''Node)
$(makeLenses ''Transaction)

instance FromJSON Transaction where
    parseJSON (Object v) =
        Transaction <$> return 0
                    <*> return 0
                    <*> return 0
                    <*> v .: "index"
                    <*> v .: "up"
                    <*> v .: "down"
                    <*> v .: "timestamp"
                    <*> return []
    parseJSON _ = mzero

data AppState = AppState 
    { _sqlPool :: ConnectionPool 
    , _salt    :: Salt } 

$(makeLenses ''AppState)

type AppController = Controller AppState

instance ToJSON Node where
    toJSON node = object
        [ "id"        .= (node & _nodeId)
        , "name"      .= (node & _name) 
        , "type"      .= (node & _family & toText) 
        , "targets"   .= (node & _targets) 
        , "syncPoint" .= (node & _syncPoint) 
        ]

instance ToJSON Transaction where
    toJSON t = object
        [ "nodeId"     .= (t & _sourceNodeId)
        , "commitId"   .= (t & _commitId)
        , "batchIndex" .= (t & _batchIndex)
        , "up"         .= (t & _upAction) 
        , "down"       .= (t & _downAction)
        , "timestamp"  .= (t & _timestamp)
        , "range"      .= (t & _range)
        ]

respondWith :: ToJSON a => Status -> a -> AppController ()
respondWith status object = respond (responseLBS status headers body)
  where
    headers = [("Content-type", "application/json")]
    body = encode object

data OkResponse = JsonOk (Maybe Value)
    deriving (Show)

data ErrorResponse = JsonError Text
    deriving (Show)

instance ToJSON OkResponse where
    toJSON (JsonOk body) = object $ 
        [ "status"  .= String "success"
        , "message" .= String "OK" 
        ] ++ maybeToList (msgBody <$> body)
      where
        msgBody body = "body" .= body

instance ToJSON ErrorResponse where
    toJSON (JsonError code) = object
        [ ("status" , "error")
        , ("error"  , String code) ]

okResponse :: Object -> OkResponse
okResponse = JsonOk . Just . Object  

data SyncRequest = SyncRequest
    { reqSyncTargets :: [Text]
    , reqSyncPoint   :: Int
    , reqSyncLog     :: [Transaction]
    } deriving (Show)

instance FromJSON SyncRequest where
    parseJSON (Object v) =
        SyncRequest <$> v .: "targets"
                    <*> v .: "syncPoint" 
                    <*> v .: "commit"
    parseJSON _ = mzero

data SyncResponse = SyncResponse
    { respRewind    :: [Command]
    , respForward   :: [Command]
    , respSyncPoint :: SyncPoint
    } deriving (Show)

instance ToJSON SyncResponse where
    toJSON SyncResponse{..} = object
        [ "reverse"   .= respRewind
        , "forward"   .= respForward
        , "syncPoint" .= respSyncPoint 
        , "status"    .= String "success" ]

