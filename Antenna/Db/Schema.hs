{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Antenna.Db.Schema 
    ( NewNode(..)
    , Node(..)
    , ResultInsert(..)
    , ResultUpdate(..)
    , SqlT
    , Transaction(..)
    , UpdateNode(..)
    , addToTransactionRange 
    , addToTransactionRange_
    , countNodes
    , deleteAllTransactions
    , deleteNode 
    , getForwardActions
    , getMaxCommitId
    , getNodeById
    , getNodeById_
    , getNodeByName
    , getNodeCount
    , getTransactionCount
    , getNodeSyncPoint
    , getNodes 
    , getReverseActions
    , getTransactionsPage 
    , hasDevice
    , insertNode 
    , insertTransaction 
    , isSuccessIns
    , lookupCredentials
    , migrateAll
    , selectNodeById
    , selectNodeCollection
    , selectNodes
    , setNodeSyncPoint
    , setNodeTargets 
    , toKey
    , unKey
    , updateNode
    , updateTimestamp
    ) where

import Control.Applicative                           ( (<$>), (<*>) )
import Control.Lens                                  ( Cons, Setting, _2, over, cons, at, (?~), (%~), (&) )
import Control.Monad                                 ( liftM, when, void )
import Control.Monad.Logger
import Control.Monad.Trans                           ( liftIO )
import Control.Monad.Trans.Resource
import Data.Aeson                                    ( FromJSON, decode, encode )
import Data.List                                     ( sortBy )
import Data.Map.Strict                               ( Map, empty, keys, elems )
import Data.Maybe                                    ( listToMaybe, fromJust, fromMaybe, isNothing, isJust )
import Data.Text                                     ( Text )
import Data.Text.Encoding                            ( encodeUtf8, decodeUtf8 )
import Data.Traversable                              ( sequence )
import Database.Persist                              ( selectList )
import Database.Persist.TH

import qualified Data.Function                    as Fun
import qualified Antenna.Types                    as T 
import qualified Data.ByteString.Lazy             as BL
import qualified Data.Map.Strict                  as MapS

import Database.Esqueleto                     hiding ( isNothing )
import Prelude                                hiding ( sequence )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Node 
        name           Text
        family         Text
        syncPoint      Int
        saturated      Bool
        locked         Bool
        deriving Eq Show
    Target 
        nodeId         NodeId 
        key            NodeId 
        deriving Eq Show
    Device 
        nodeId         NodeId 
        secret         Text
        deriving Eq Show
    Transaction
        nodeId         NodeId
        commitId       Int
        batchIndex     Int
        upMethod       Text
        upResource     Text
        upPayload      Text
        downMethod     Text
        downResource   Text
        downPayload    Text
        timestamp      Int
        deriving Eq Show
    Range
        transactionId  TransactionId
        nodeId         NodeId
        deriving Eq Show
|]

type SqlT = SqlPersistT (ResourceT (NoLoggingT IO))

data ResultInsert a
  = InsertSuccess (Key a)
  | InsertConflict 
  | InsertBadRequest

isSuccessIns :: ResultInsert a -> Bool
isSuccessIns (InsertSuccess _) = True
isSuccessIns _________________ = False

data ResultUpdate
  = UpdateSuccess
  | UpdateNotFound

unKey :: (ToBackendKey SqlBackend a) => Key a -> Int
unKey = fromIntegral . unSqlBackendKey . toBackendKey 

toKey :: (Integral a, ToBackendKey SqlBackend b) => a -> Key b
toKey = toSqlKey . fromIntegral 

type Modifier k a b = a -> MapS.Map k (Int, b) -> MapS.Map k (Int, b)

injectKeys :: [b] -> Modifier k (Int, b) d -> Modifier k e d -> [e] -> [d]
injectKeys items construct insert x = snd <$> sorted
  where
    wkeys  = foldr insert init x
    sorted = sortBy (compare `Fun.on` fst) (elems wkeys)
    init   = foldr construct empty (zip [ 1 .. ] items)

selectNodes :: SqlT [Entity Node]
selectNodes = select $ from $ 
    \node -> do
        orderBy [asc (node ^. NodeId)]
        return node
 
selectNodeCollection :: [Text] -> SqlT [Value (Key Node)]
selectNodeCollection names = select $ from $ 
    \node -> do
        where_ $ node ^. NodeName `in_` valList names
        return (node ^. NodeId)

selectNodeTargets :: Key Node -> SqlT [(Entity Target, Entity Node)]
selectNodeTargets nodeId = select $ from $
    \(target `InnerJoin` node) -> do
        on (target ^. TargetKey ==. node ^. NodeId)
        where_ $ target ^. TargetNodeId ==. val nodeId
        return (target, node)

selectNodeTargetsIn :: [Key Node] -> SqlT [(Entity Target, Entity Node)]
selectNodeTargetsIn nodes = select $ from $
    \(target `InnerJoin` node) -> do
        on (target ^. TargetKey ==. node ^. NodeId)
        where_ $ target ^. TargetNodeId `in_` valList nodes
        return (target, node)

getNodes :: SqlT [T.Node]
getNodes = do
    nodes <- selectNodes
    nodeTargets <- selectNodeTargetsIn (entityKey <$> nodes)
    return $ injectKeys nodes construct insert nodeTargets 
  where
    construct (index, node) = 
        let key = entityKey node
            val = entityVal node
         in MapS.insert key (index, T.Node 
                { T._nodeId    = unKey key
                , T._name      = val & nodeName
                , T._family    = T.toType (val & nodeName)
                , T._targets   = [] 
                , T._syncPoint = if val & nodeSaturated 
                                    then T.Saturated 
                                    else T.AtTime (T.Timestamp $ fromIntegral $ val & nodeSyncPoint)
                , T._locked    = val & nodeLocked })
    insert (target,node) = 
        let name = nodeName (entityVal node)
            targetId = entityVal target & targetNodeId
         in MapS.update (Just . over (_2 . T.targets) (cons name)) targetId

lookupCredentials :: Text -> Text -> SqlT (Maybe T.Node)
lookupCredentials name password = do
    result <- select $ from $ 
        \(node `InnerJoin` device) -> do
            on (node ^. NodeId ==. device ^. DeviceNodeId)
            where_ $ device ^. DeviceSecret ==. val password &&. node ^. NodeName  ==. val name
            return node
    sequence (translateNode <$> listToMaybe result)

selectNodeById :: Key Node -> SqlT (Maybe (Entity Node))
selectNodeById nodeId = do
    result <- select $ from $ 
        \node -> do
            where_ $ node ^. NodeId ==. val nodeId
            return node
    return (listToMaybe result)

getNodeByName :: Text -> SqlT (Maybe T.Node)
getNodeByName name = do
    result <- select $ from $ 
        \node -> do
            where_ $ node ^. NodeName ==. val name
            return node
    sequence (translateNode <$> listToMaybe result)

getNodeById_ :: Int -> SqlT (Maybe T.Node)
getNodeById_ = getNodeById . toKey 

getNodeById :: Key Node -> SqlT (Maybe T.Node)
getNodeById nodeId = do
    maybeNode <- selectNodeById nodeId
    sequence (translateNode <$> maybeNode)

translateNode :: Entity Node -> SqlT T.Node
translateNode node = do
    let key = entityKey node
        val = entityVal node
    targets <- selectNodeTargets key
    return T.Node 
        { T._nodeId    = unKey key
        , T._name      = val & nodeName
        , T._family    = T.toType (val & nodeName)
        , T._targets   = nodeName . entityVal . snd <$> targets 
        , T._syncPoint = if val & nodeSaturated 
                            then T.Saturated 
                            else T.AtTime (T.Timestamp $ fromIntegral $ val & nodeSyncPoint)
        , T._locked    = val & nodeLocked }

selectTransactionsPage :: Int -> Int -> SqlT [Entity Transaction]
selectTransactionsPage offs lim = 
     select $ from $ \transaction -> do
         offset (fromIntegral offs)
         limit (fromIntegral lim)
         orderBy [desc (transaction ^. TransactionCommitId), desc (transaction ^. TransactionBatchIndex)]
         return transaction

selectReverseTransactions :: Key Node -> T.SyncPoint -> SqlT [Entity Transaction]
selectReverseTransactions node T.Saturated = return []
selectReverseTransactions node (T.AtTime (T.Timestamp ts)) = 
    select $ from $ \(transaction `InnerJoin` range) -> do
        on (range ^. RangeTransactionId ==. transaction ^. TransactionId)
        where_ (range ^. RangeNodeId ==. val node &&. transaction ^. TransactionTimestamp >=. val timestamp)
        orderBy [desc (transaction ^. TransactionTimestamp) ]
        return transaction
  where
    timestamp = fromIntegral ts

selectForwardTransactions :: [Key Node] -> T.SyncPoint -> SqlT [Entity Transaction]
selectForwardTransactions nodes T.Saturated = return []
selectForwardTransactions nodes (T.AtTime (T.Timestamp ts)) = 
    select $ from $ \(transaction `InnerJoin` range) -> do
        on (range ^. RangeTransactionId ==. transaction ^. TransactionId)
        groupBy (transaction ^. TransactionId)
        where_ (range ^. RangeNodeId `in_` valList nodes &&. transaction ^. TransactionTimestamp >=. val timestamp)
        orderBy [asc (transaction ^. TransactionTimestamp) ]
        return transaction
  where
    timestamp = fromIntegral ts

selectRange :: [Key Transaction] -> SqlT [(Entity Range, Entity Node)]
selectRange transactions = 
    select $ from $
        \(range `InnerJoin` node) -> do
            on (range ^. RangeNodeId ==. node ^. NodeId)
            where_ $ range ^. RangeTransactionId `in_` valList transactions
            return (range, node)

populateTransactions :: [Entity Transaction] -> SqlT [T.Transaction]
populateTransactions transactions = do
    range <- selectRange (entityKey <$> transactions)
    return $ injectKeys transactions construct insert range
  where
    construct :: (Int, Entity Transaction)
              -> Map (Key Transaction) (Int, T.Transaction)
              -> Map (Key Transaction) (Int, T.Transaction)
    construct (index, transaction) = 
        let key  = entityKey transaction
            val  = entityVal transaction
            up   = T.Command 
                    { _method   = readMethod (val & transactionUpMethod)
                    , _resource = val & transactionUpResource
                    , _payload  = decoded (val & transactionUpPayload) 
                    }
            down = T.Command 
                    { _method   = readMethod (val & transactionDownMethod)
                    , _resource = val & transactionDownResource
                    , _payload  = decoded (val & transactionDownPayload) 
                    } 
         in MapS.insert key (index, T.Transaction 
            { T._transactionId  = unKey key
            , T._sourceNodeId   = val & transactionNodeId & unKey
            , T._commitId       = val & transactionCommitId
            , T._batchIndex     = val & transactionBatchIndex
            , T._upAction       = up
            , T._downAction     = down
            , T._timestamp      = T.Timestamp (fromIntegral $ val & transactionTimestamp)
            , T._range          = [] })
    insert (range, node) = 
        let name = nodeName (entityVal node)
            transId = rangeTransactionId $ entityVal range
         in MapS.update (Just . over (_2 . T.range) (cons name)) transId
    decoded :: FromJSON a => Text -> Maybe a
    decoded = decode . BL.fromStrict . encodeUtf8 
    readMethod "PUT"    = T.PUT
    readMethod "PATCH"  = T.PATCH
    readMethod "DELETE" = T.DELETE
    readMethod _        = T.POST

getTransactionsPage :: Int -> Int -> SqlT [T.Transaction]
getTransactionsPage offs lim = selectTransactionsPage offs lim >>= populateTransactions 

getForwardActions :: [Key Node] -> T.SyncPoint -> SqlT [T.Transaction]
getForwardActions nodes ts = selectForwardTransactions nodes ts >>= populateTransactions 

getReverseActions :: Key Node -> T.SyncPoint -> SqlT [T.Transaction]
getReverseActions node ts = selectReverseTransactions node ts >>= populateTransactions 

countNodes :: Text -> SqlT [Value Int]
countNodes name = 
    select $ from $ \node -> do
        where_ (node ^. NodeName ==. val name)
        return countRows

type Count a = SqlExpr (Entity a) -> SqlQuery (SqlExpr (Value Int))

getNodeCount :: SqlT Int
getNodeCount = do
    let items = const (return countRows) :: Count Node
    select (from items) >>= \case
      [Value n] -> return n
      _________ -> error "SQL error."

getTransactionCount :: SqlT Int
getTransactionCount = do
    let items = const (return countRows) :: Count Transaction
    select (from items) >>= \case
      [Value t] -> return t 
      _________ -> error "SQL error."

data NewNode = NewNode
    { newName   :: Text
    , newFamily :: T.NodeType 
    , newPass   :: Maybe Text
    , newLocked :: Bool
    } deriving (Show)

insertNode :: NewNode -> SqlT (ResultInsert Node)
insertNode node 
    | isDevice && isNothing (node & newPass) = return InsertBadRequest
    | otherwise = countNodes nodeName >>= go
  where
    nodeName = node & newName
    isDevice = T.Device == (node & newFamily)
    go [Value 0] = do
        key <- insert $ Node 
            nodeName 
            (T.toText $ node & newFamily) 
            0
            True
            (node & newLocked)
        insertSelect $ from $ \node -> do
            where_ $ node ^. NodeId !=. val key
            return $ Target <# val key <&> (node ^. NodeId)
        when isDevice $ 
            let secret = fromJust (node & newPass)
             in void $ insert (Device key secret) 
        return (InsertSuccess key)
    go _ = return InsertConflict -- A node with the given name already exists

data UpdateNode = UpdateNode
    { updateName    :: Maybe Text
    , updatePass    :: Maybe Text
    , updateTargets :: Maybe [Text]
    } deriving (Show)

updateNode :: Key Node -> UpdateNode -> SqlT ResultUpdate
updateNode nodeId UpdateNode{..} = do
    maybeNode <- selectNodeById nodeId
    case maybeNode of
      Nothing -> return UpdateNotFound
      Just old -> do
        let nodeVal = entityVal old
            oldName = nodeName nodeVal
        update $ \node -> do
            set node [ NodeName =. val (fromMaybe oldName updateName) ]
            where_ $ node ^. NodeId ==. val nodeId 
        void $ sequence (setNodeTargets <$> Just nodeId <*> updateTargets)
        when (nodeFamily nodeVal == "device" && isJust updatePass) $ do
            let secret = fromJust updatePass
            update $ \device -> do
                set device [ DeviceSecret =. val secret ]
                where_ $ device ^. DeviceNodeId ==. val nodeId
        return UpdateSuccess

deleteNode :: Key Node -> SqlT ()
deleteNode nodeId = do
    nodes <- select $ from $ \node -> do
        where_ (node ^. NodeId ==. val nodeId)
        return node
    let nodeKeyList = valList $ entityKey <$> nodes
    delete $ from $ \target ->
        where_ $ target ^. TargetNodeId `in_` nodeKeyList ||. target ^. TargetKey `in_` nodeKeyList
    delete $ from $ \device ->
        where_ $ device ^. DeviceNodeId `in_` nodeKeyList 
    delete $ from $ \node ->
        where_ $ node ^. NodeId `in_` nodeKeyList

setNodeTargets :: Key Node -> [Text] -> SqlT ()
setNodeTargets nodeId targets = do
    maybeNode <- selectNodeById nodeId
    case maybeNode of
      Nothing -> return ()
      Just node -> do
        let nodeKey = entityKey node
            targetList = valList targets
        delete $ from $ \target ->          -- Delete existing targets
            where_ $ target ^. TargetNodeId ==. val nodeKey
        insertSelect $ from $ \node -> do   -- Insert new targets
            where_ $ node ^. NodeName `in_` targetList
            return $ Target <# val nodeKey <&> (node ^. NodeId)
 
insertTransaction :: Key Node     -- ^ Node id
                  -> Int          -- ^ Commit id
                  -> Int          -- ^ Batch index
                  -> Text         -- ^ Up method
                  -> Text         -- ^ Up resource
                  -> Text         -- ^ Up payload
                  -> Text         -- ^ Down method
                  -> Text         -- ^ Down resource
                  -> Text         -- ^ Down payload
                  -> T.Timestamp  -- ^ Timestamp
                  -> SqlT (Maybe (Key Transaction))
insertTransaction nodeId commitId batchIndex upMethod upResource upPayload downMethod downResource downPayload (T.Timestamp ts) = do
    maybeNode <- selectNodeById nodeId
    case maybeNode of
      Nothing -> return Nothing
      Just node -> do
        let nodeKey = entityKey node
        liftM Just $ insert (Transaction nodeId commitId batchIndex upMethod upResource upPayload downMethod downResource downPayload timestamp) 
  where
    timestamp = fromIntegral ts

addToTransactionRange :: Key Transaction -> Key Node -> SqlT (Maybe (Key Range))
addToTransactionRange transactionId nodeId = do
    maybeNode <- selectNodeById nodeId
    case maybeNode of
      Nothing -> return Nothing
      Just node -> do
        let nodeKey = entityKey node
        liftM Just $ insert $ Range transactionId nodeKey

addToTransactionRange_ :: [Key Transaction] -> Key Node -> SqlT ()
addToTransactionRange_ transactions nodeId = insertMany_ vals
  where 
    vals = [ Range transactionId nodeId | transactionId <- transactions ]

type Delete a = SqlExpr (Entity a) -> SqlQuery ()

deleteAllTransactions :: SqlT ()
deleteAllTransactions = 
    delete (from range) >> delete (from transaction)
  where
    range :: Delete Range
    range _ = return ()
    transaction :: Delete Transaction
    transaction _ = return ()

hasDevice :: Text -> Text -> SqlT Bool
hasDevice name secret = query >>= \case 
    [Value 1] -> return True
    _________ -> return False
  where
    query :: SqlT [Value Int]
    query = select $ from $
        \(node `InnerJoin` device) -> do
            on (node ^. NodeId ==. device ^. DeviceNodeId)
            where_ $ node ^. NodeName ==. val name &&. device ^. DeviceSecret ==. val secret
            return countRows

updateTimestamp :: Int -> SqlT [Text]
updateTimestamp ts = do
    nodes <- selectNodes
    let updated = filter predicate nodes
    update $ \node -> do
        set node [ NodeSaturated =. val False
                 , NodeSyncPoint =. val ts ]
        where_ $ node ^. NodeId `in_` valList (entityKey <$> updated)
    return (nodeName_ <$> updated)
  where
    predicate node = nodeSaturated nodeVal || nodeSyncPoint nodeVal > ts
      where nodeVal = entityVal node 
    nodeName_ = nodeName . entityVal

getNodeSyncPoint :: Key Node -> SqlT T.SyncPoint
getNodeSyncPoint nodeId = do
    result <- select $ from $ 
        \node -> do
            where_ $ node ^. NodeId ==. val nodeId
            limit 1
            return node
    case result of
      [node] -> 
        let val = entityVal node
         in return $ if val & nodeSaturated 
                then T.Saturated 
                else T.AtTime (T.Timestamp $ fromIntegral $ val & nodeSyncPoint)
      _ -> error "Application error."
 
setNodeSyncPoint :: Key Node -> SqlT T.SyncPoint
setNodeSyncPoint nodeId = do
    point <- select $ from $
        \(transaction `LeftOuterJoin` range) -> do
            on (range ?. RangeTransactionId ==. just (transaction ^. TransactionId) 
                &&. range ?. RangeNodeId ==. just (val nodeId))
            where_ $ range ?. RangeTransactionId ==. nothing
            orderBy [desc $ transaction ^. TransactionTimestamp ]
            limit 1
            return (transaction ^. TransactionTimestamp)
    case point of
      [Value t] -> do
        update $ \node -> do
            set node [ NodeSaturated =. val False
                     , NodeSyncPoint =. val t 
                     ]
            where_ $ node ^. NodeId ==. val nodeId
        return undefined
      _ -> do
        update $ \node -> do
            set node [ NodeSaturated =. val True
                     , NodeSyncPoint =. val 0 
                     ]
            where_ $ node ^. NodeId ==. val nodeId
        return T.Saturated

getMaxCommitId :: SqlT Int
getMaxCommitId = do
    maxId <- select $ from $ \transaction -> do
        limit 1
        let maxId = max_ (transaction ^. TransactionCommitId)
        return maxId
    return $ case maxId of
      [Value (Just v)] -> v
      _ -> 0
 
