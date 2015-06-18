{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Antenna.Db.Schema 
    ( getNodes 
    , getTransactionsPage 
    , getTransactionsGte 
    , insertNode 
    , deleteNode 
    , replaceNodeTargets 
    , insertTransaction 
    , addToTransactionRange 
    , countNodes
    , migrateAll
    ) where

import Control.Applicative                           ( (<$>) )
import Control.Lens                                  ( Cons, Setting, set, over, cons, at, (?~), (%~), (&) )
import Control.Monad                                 ( liftM )
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Map.Strict                               ( Map, empty, keys, elems )
import Data.Text                                     ( Text )
import Database.Esqueleto
import Database.Persist                              ( selectList )
import Database.Persist.TH

import qualified Antenna.Types                    as T 
import qualified Data.Map.Strict                  as MapS

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Node 
        name           Text
        deriving Eq Show
    Target 
        nodeId         NodeId 
        key            NodeId 
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
 
selectNodeTargets :: [Key Node] -> SqlT [(Entity Target, Entity Node)]
selectNodeTargets nodes = select $ from $
    \(target `InnerJoin` node) -> do
        on (target ^. TargetNodeId ==. node ^. NodeId)
        where_ $ target ^. TargetNodeId `in_` valList nodes
        return (target, node)
 
selectNodeByName :: Text -> SqlT (Maybe (Entity Node))
selectNodeByName name = do
    result <- select $ from $ 
        \node -> do
            where_ $ node ^. NodeName ==. val name
            return node
    return $ case result of
      (node:_) -> Just node
      _ -> Nothing

getNodes :: SqlT [T.Node]
getNodes = do
    nodes <- selectNodes
    nodeTargets <- selectNodeTargets (entityKey <$> nodes)
    injectKeys nodes construct insert nodeTargets 
  where
    construct node = 
        let key = entityKey node
            val = entityVal node
         in MapS.insert key $ T.Node (unKey key) (val & nodeName) []
    insert (_,node) = 
        let name = nodeName (entityVal node)
         in MapS.update (Just . over T.targets (cons name)) (entityKey node) 

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

insertNode :: Text -> SqlT (Maybe (Key Node))
insertNode name = do
    count <- countNodes name
    case count of
      [Value 0] -> do
          newNodeId <- insert $ Node name
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
        where_ $ target ^. TargetNodeId `in_` nodeKeyList
    delete $ from $ \node ->
        where_ $ node ^. NodeId `in_` nodeKeyList

replaceNodeTargets :: Text -> [Text] -> SqlT ()
replaceNodeTargets nodeName targets = do
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

