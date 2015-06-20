{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Antenna.Db.Schema 
    ( NewNode(..)
    , SqlT
    , addToTransactionRange 
    , countNodes
    , deleteAllTransactions
    , deleteNode 
    , getNodeByName
    , getNodeCount
    , getNodes 
    , getTransactionsGte 
    , getTransactionsPage 
    , hasDevice
    , insertDevice
    , insertNode 
    , insertTransaction 
    , lookupDevice
    , migrateAll
    , selectNodeByName
    , setNodeTargets 
    ) where

import Control.Monad.IO.Class                        ( liftIO )

import Control.Applicative                           ( (<$>), (<*>) )
import Control.Lens                                  ( Cons, Setting, set, over, cons, at, (?~), (%~), (&) )
import Control.Monad                                 ( liftM )
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Crypto.PasswordStore
import Data.Map.Strict                               ( Map, empty, keys, elems )
import Data.Maybe                                    ( listToMaybe )
import Data.Text                                     ( Text )
import Data.Text.Encoding                            ( encodeUtf8, decodeUtf8 )
import Data.Traversable                              ( sequence )
import Database.Esqueleto
import Database.Persist                              ( selectList )
import Database.Persist.TH

import qualified Antenna.Types                    as T 
import qualified Data.Map.Strict                  as MapS

import Prelude                                hiding ( sequence )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Node 
        name           Text
        family         Text
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
        upAction       Text
        downAction     Text
        timestamp      Int
        deriving Eq Show
    Range
        transactionId  TransactionId
        nodeId         NodeId
        deriving Eq Show
|]

type SqlT = SqlPersistT (ResourceT (NoLoggingT IO))
type Timestamp = Int

unKey :: (ToBackendKey SqlBackend a) => Key a -> Int
unKey = fromIntegral . unSqlBackendKey . toBackendKey 

injectKeys :: Monad m => [a] -> (a -> Map k b -> Map k b) -> (c -> Map k b -> Map k b) -> [c] -> m [b]
injectKeys items construct insert = 
    let init = foldr construct empty items 
     in return . elems . foldr insert init

selectNodes :: SqlT [Entity Node]
selectNodes = select $ from $ 
    \node -> do
        orderBy [asc (node ^. NodeId)]
        return node
 
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
    injectKeys nodes construct insert nodeTargets 
  where
    construct node = 
        let key = entityKey node
            val = entityVal node
         in MapS.insert key T.Node 
                { T._nodeId  = unKey key
                , T._name    = val & nodeName
                , T._family  = T.toType (val & nodeName)
                , T._targets = [] }
    insert (target,node) = 
        let name = nodeName (entityVal node)
            targetId = entityVal target & targetNodeId
         in MapS.update (Just . over T.targets (cons name)) targetId

selectNodeByName :: Text -> SqlT (Maybe (Entity Node))
selectNodeByName name = do
    result <- select $ from $ 
        \node -> do
            where_ $ node ^. NodeName ==. val name
            return node
    return (listToMaybe result)

getNodeByName :: Text -> SqlT (Maybe T.Node)
getNodeByName name = do
    maybeNode <- selectNodeByName name
    sequence $ translate <$> maybeNode
  where
    translate :: Entity Node -> SqlT T.Node
    translate node = do
        let key = entityKey node
            val = entityVal node
        targets <- selectNodeTargets key
        return T.Node 
            { T._nodeId  = unKey key
            , T._name    = val & nodeName
            , T._family  = T.toType (val & nodeName)
            , T._targets = nodeName . entityVal . snd <$> targets }

selectTransactionsPage :: Int -> Int -> SqlT [Entity Transaction]
selectTransactionsPage offs lim = 
     select $ from $ \transaction -> do
         offset (fromIntegral offs)
         limit (fromIntegral lim)
         orderBy [asc (transaction ^. TransactionTimestamp)]
         return transaction

selectTransactionsGte :: Timestamp -> SqlT [Entity Transaction]
selectTransactionsGte ts = 
    select $ from $ \transaction -> do
        where_ (transaction ^. TransactionTimestamp >=. val ts)
        orderBy [asc (transaction ^. TransactionTimestamp)]
        return transaction

selectTransactionsRange :: [Key Transaction] -> SqlT [(Entity Range, Entity Node)]
selectTransactionsRange transactions = 
    select $ from $
        \(range `InnerJoin` node) -> do
            on (range ^. RangeNodeId ==. node ^. NodeId)
            where_ $ range ^. RangeTransactionId `in_` valList transactions
            return (range, node)

populateTransactions :: [Entity Transaction] -> SqlT [T.Transaction]
populateTransactions transactions = do
    range <- selectTransactionsRange (entityKey <$> transactions)
    injectKeys transactions construct insert range
  where
    construct transaction = 
        let key = entityKey transaction
            val = entityVal transaction
            _id = unKey key
         in MapS.insert key T.Transaction 
                { T._transactionId = _id
                , T._upAction      = val & transactionUpAction 
                , T._downAction    = val & transactionDownAction 
                , T._timestamp     = val & transactionTimestamp
                , T._range         = [] }
    insert (range,node) = 
        let name = nodeName (entityVal node)
            transId = rangeTransactionId $ entityVal range
         in MapS.update (Just . over T.range (cons name)) transId

getTransactionsPage :: Int -> Int -> SqlT [T.Transaction]
getTransactionsPage offs lim = selectTransactionsPage offs lim >>= populateTransactions 

getTransactionsGte :: Timestamp -> SqlT [T.Transaction]
getTransactionsGte ts = selectTransactionsGte ts >>= populateTransactions 

countNodes :: Text -> SqlT [Value Int]
countNodes name = 
    select $ from $ \node -> do
        where_ (node ^. NodeName ==. val name)
        return countRows

getNodeCount :: SqlT Int
getNodeCount = 
    select (from nodes) >>= \case
      [Value n] -> return n
      _________ -> error "SQL error."
  where
    nodes :: Num a => SqlExpr (Entity Node) -> SqlQuery (SqlExpr (Value a))
    nodes = const (return countRows)

data NewNode = NewNode
    { newName   :: Text
    , newFamily :: T.NodeType }

insertNode :: NewNode -> SqlT (Maybe (Key Node))
insertNode node = do
    count <- countNodes (node & newName)
    case count of
      [Value 0] -> do
          newNodeId <- insert $ Node 
            (node & newName) 
            (T.toText $ node & newFamily) 
          insertSelect $ from $ \node -> 
              return $ Target <# val newNodeId <&> (node ^. NodeId)
          return $ Just newNodeId
      _ -> return Nothing  -- A node with the provided name already exists

deleteNode :: Text -> SqlT ()
deleteNode name = do
    nodes <- select $ from $ \node -> do
        where_ (node ^. NodeName ==. val name)
        return node
    let nodeKeyList = valList $ entityKey <$> nodes
    delete $ from $ \target ->
        where_ $ target ^. TargetNodeId `in_` nodeKeyList ||. target ^. TargetKey `in_` nodeKeyList
    delete $ from $ \device ->
        where_ $ device ^. DeviceNodeId `in_` nodeKeyList 
    delete $ from $ \node ->
        where_ $ node ^. NodeId `in_` nodeKeyList

setNodeTargets :: Text -> [Text] -> SqlT ()
setNodeTargets nodeName targets = do
    maybeNode <- selectNodeByName nodeName
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
 
insertTransaction :: Text -> Text -> Text -> Timestamp -> SqlT (Maybe (Key Transaction))
insertTransaction nodeName upAction downAction timestamp = do
    maybeNode <- selectNodeByName nodeName
    case maybeNode of
      Nothing -> return Nothing
      Just node -> do
        let nodeKey = entityKey node
        liftM Just $ insert (Transaction nodeKey upAction downAction timestamp) 

addToTransactionRange :: Key Transaction -> Text -> SqlT (Maybe (Key Range))
addToTransactionRange transactionId nodeName = do
    maybeNode <- selectNodeByName nodeName
    case maybeNode of
      Nothing -> return Nothing
      Just node -> do
        let nodeKey = entityKey node
        liftM Just $ insert $ Range transactionId nodeKey

type Delete a = SqlExpr (Entity a) -> SqlQuery ()

deleteAllTransactions :: SqlT ()
deleteAllTransactions = 
    delete (from range) >> delete (from transaction)
  where
    range :: Delete Range
    range _ = return ()
    transaction :: Delete Transaction
    transaction _ = return ()

makePwd :: Text -> Salt -> Text
makePwd pass salt = decodeUtf8 $ makePasswordSalt (encodeUtf8 pass) salt 17

lookupDevice :: Text -> Text -> Salt -> SqlT (Maybe T.Node)
lookupDevice name pass salt = hasDevice name pass salt >>= \exists -> 
    if exists 
        then getNodeByName name
        else return Nothing

hasDevice :: Text -> Text -> Salt -> SqlT Bool
hasDevice name pass salt = query >>= \case 
    [Value 1] -> return True
    _________ -> return False
  where
    query :: SqlT [Value Int]
    query = select $ from $
        \(node `InnerJoin` device) -> do
            on (node ^. NodeId ==. device ^. DeviceNodeId)
            where_ $ node ^. NodeName ==. val name &&. device ^. DeviceSecret ==. val secret
            return countRows
    secret = makePwd pass salt

insertDevice :: Text -> Text -> Salt -> SqlT (Maybe (Key Device))
insertDevice name plaintext salt = do
    let secret = makePwd plaintext salt
    maybeNodeId <- insertNode $ NewNode name T.Device
    case maybeNodeId of
      Just key -> do
        deviceId <- insert (Device key secret) 
        return (Just deviceId)
      Nothing -> return Nothing

