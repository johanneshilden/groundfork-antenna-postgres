{-# LANGUAGE OverloadedStrings #-}
module Antenna.Tests 
    ( runTests
    , createRootUser
    ) where

import Antenna.Db
import Antenna.Db.Schema
import Antenna.Types
import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class                        ( liftIO )
import Crypto.PasswordStore
import Data.Maybe                                    ( fromJust, isJust )
import Database.Persist.Postgresql

import qualified Data.ByteString.Char8            as C8

testAssert :: Bool -> String -> String -> IO ()
testAssert True test message = print $ test ++ " Ok!" 
testAssert ____ test message = print message

assert :: Bool -> String -> String -> SqlT ()
assert a b = liftIO . testAssert a b 

allElems :: Eq a => [a] -> [a] -> Bool
allElems xs ys 
    | length xs /= length ys = False
    | otherwise = foldr go True xs
  where
    go y p | not p       = False
           | otherwise = y `elem` ys

createRootUser :: IO ()
createRootUser = do
    pool <- inIO $ createPostgresqlPool connectionStr 10
    let salt = makeSalt "Mxg4YN0OaE3xaehmg3up"
    runDb pool $ insertNode $ NewNode "root" Device (Just $ makePwd "root" salt) False
    return ()


runTests :: IO ()
runTests = do
    pool <- inIO $ createPostgresqlPool connectionStr 10
    let salt = makeSalt "Mxg4YN0OaE3xaehmg3up"

    runDb pool $ do

        runMigration migrateAll

        rawExecute "DELETE FROM range"       [ ]
        rawExecute "DELETE FROM transaction" [ ]
        rawExecute "DELETE FROM device"      [ ]
        rawExecute "DELETE FROM target"      [ ]
        rawExecute "DELETE FROM node"        [ ]

        alice <- insertNode $ NewNode "alice" Device (Just $ makePwd "xxx" salt) False
        bob   <- insertNode $ NewNode "bob"   Device (Just $ makePwd "bob" salt) False
    
        assert (isSuccessIns alice) "Test 1.1" "Expected isSuccess alice == True, instead got False"
        assert (isSuccessIns bob)   "Test 1.2" "Expected isSuccess bob == True, instead got False"

        let InsertSuccess aliceId = alice
        let InsertSuccess bobId = bob

        setNodeTargets aliceId ["bob"]
        setNodeTargets bobId   ["alice"]

        nodeCount <- getNodeCount
        assert (nodeCount == 2) "Test 1.3" ("Expected getNodeCount == 2, instead got " ++ show nodeCount)

        maybeNode <- getNodeById aliceId
        assert (_name `fmap` maybeNode == Just "alice") "Test 2" ("Expected (Just 'alice'), instead got " ++ show (_name `fmap` maybeNode))
        assert (_targets `fmap` maybeNode == Just ["bob"]) "Test 3" ("Expected (Just '[\"bob\"]'), instead got " ++ show (_targets `fmap` maybeNode))

        hasd <- hasDevice "alice" (makePwd "xxx" salt) 
        assert hasd "Test 4" "Expected hasDevice 'alice' 'xxx' == True, instead got False"

        hasd <- hasDevice "alice" (makePwd "pogostick" salt) 
        assert (not hasd) "Test 5" "Expected hasDevice 'alice' 'pogostick' == False, instead got True"

        hasd <- hasDevice "what" (makePwd "not" salt) 
        assert (not hasd) "Test 6" "Expected hasDevice 'what' 'not' == False, instead got True"

        deleteNode bobId
        nodeCount <- getNodeCount
        assert (nodeCount == 1) "Test 7" ("Expected getNodeCount == 1, instead got " ++ show nodeCount)

        hasd <- hasDevice "bob" (makePwd "bob" salt) 
        assert (not hasd) "Test 8" "Expected hasDevice 'bob' 'bob' == False, instead got True"

        bob <- insertNode $ NewNode "bob" Device (Just $ makePwd "bob" salt) False

        assert (isSuccessIns bob) "Test 9.1" "Expected isSuccess bob == True, instead got False"
        let InsertSuccess bobId = bob

        updateNode bobId $ UpdateNode (Just "rob") Nothing Nothing

        hasd <- hasDevice "bob" (makePwd "bob" salt) 
        assert (not hasd) "Test 9.2" "Expected hasDevice 'bob' 'bob' == False, instead got True"

        hasd <- hasDevice "rob" (makePwd "bob" salt) 
        assert hasd "Test 10" "Expected hasDevice 'rob' 'bob' == True, instead got False"

        insertNode $ NewNode "node3" Virtual Nothing False
        insertNode $ NewNode "node4" Virtual Nothing False
        insertNode $ NewNode "node5" Virtual Nothing False
        insertNode $ NewNode "node6" Virtual Nothing False
 
        nodeCount <- getNodeCount
        assert (nodeCount == 6) "Test 11" ("Expected getNodeCount == 11, instead got " ++ show nodeCount)

        rob <- getNodeByName "rob"

        assert (isJust rob) "Test 12.1" "Expected True == isJust rob, instead got False"

        let rob' = fromJust rob

        updateNode (toKey $ rob' ^. nodeId) $ UpdateNode Nothing Nothing (Just ["alice", "node5"])

        maybeNode <- getNodeByName "rob"
        assert (isJust maybeNode) "Test 12.2" "Expected True == isJust maybeNode, instead got False"

        let node = fromJust maybeNode
        assert (allElems ["alice", "node5"] $ _targets node) "Test 13" "Target list does not match assigned targets."

        updateNode (toKey $ rob' ^. nodeId) $ UpdateNode Nothing Nothing (Just ["node5", "node4", "node3", "alice"])

        maybeNode <- getNodeByName "rob"
        assert (isJust maybeNode) "Test 14" "Expected True == isJust maybeNode, instead got False"

        let node = fromJust maybeNode
        assert (allElems ["node5", "node4", "node3", "alice"] $ _targets node) "Test 15" "Target list does not match assigned targets."

        updateNode (toKey $ rob' ^. nodeId) $ UpdateNode Nothing Nothing Nothing

        maybeNode <- getNodeByName "rob"
        let node = fromJust maybeNode
        assert (allElems ["node5", "node4", "node3", "alice"] $ _targets node) "Test 16" "Target list does not match assigned targets."

        updateNode (toKey $ rob' ^. nodeId) $ UpdateNode Nothing (Just (makePwd "newpwd" salt)) Nothing

        hasd <- hasDevice "rob" (makePwd "bob" salt) 
        assert (not hasd) "Test 17" "Expected hasDevice 'rob' 'bob' == False, instead got True"

        hasd <- hasDevice "rob" (makePwd "newpwd" salt) 
        assert hasd "Test 18" "Expected hasDevice 'rob' 'newpwd' == True, instead got False"

        updateNode (toKey $ rob' ^. nodeId) $ UpdateNode Nothing (Just (makePwd "xxx" salt)) (Just [])

        hasd <- hasDevice "rob" (makePwd "xxx" salt) 
        assert hasd "Test 19" "Expected hasDevice 'rob' 'xxx' == True, instead got False"

        maybeNode <- getNodeByName "rob"
        let node = fromJust maybeNode
        assert (allElems [] $ _targets node) "Test 20" "Target list does not match assigned targets."

        -----------------------------------------------

        a <- insertNode $ NewNode "test-1" Virtual Nothing True
        b <- insertNode $ NewNode "test-2" Virtual Nothing True
        c <- insertNode $ NewNode "test-3" Virtual Nothing True
        d <- insertNode $ NewNode "test-4" Virtual Nothing True

        let InsertSuccess nodeA = a
        let InsertSuccess nodeB = b
        let InsertSuccess nodeC = c 
        let InsertSuccess nodeD = d 

        t1 <- liftA fromJust $ insertTransaction nodeA 1 1  "" "" "" "" "" "" (Timestamp 10)
        t2 <- liftA fromJust $ insertTransaction nodeA 1 2  "" "" "" "" "" "" (Timestamp 14)
        t3 <- liftA fromJust $ insertTransaction nodeA 1 3  "" "" "" "" "" "" (Timestamp 34)
        t4 <- liftA fromJust $ insertTransaction nodeA 1 4  "" "" "" "" "" "" (Timestamp 54)
                                                                        
        t5 <- liftA fromJust $ insertTransaction nodeB 2 1  "" "" "" "" "" "" (Timestamp 94)
        t6 <- liftA fromJust $ insertTransaction nodeB 2 2  "" "" "" "" "" "" (Timestamp 194)
                                                                        
        t7 <- liftA fromJust $ insertTransaction nodeC 3 1  "" "" "" "" "" "" (Timestamp 294)
        t8 <- liftA fromJust $ insertTransaction nodeC 3 2  "" "" "" "" "" "" (Timestamp 394)
        t9 <- liftA fromJust $ insertTransaction nodeC 3 3  "" "" "" "" "" "" (Timestamp 400)
        t10 <- liftA fromJust $ insertTransaction nodeC 3 4 "" "" "" "" "" "" (Timestamp 410)
                                                                        
        t11 <- liftA fromJust $ insertTransaction nodeA 4 1 "" "" "" "" "" "" (Timestamp 64)
        t12 <- liftA fromJust $ insertTransaction nodeA 4 2 "" "" "" "" "" "" (Timestamp 105)
        t13 <- liftA fromJust $ insertTransaction nodeA 4 3 "" "" "" "" "" "" (Timestamp 300)
        t14 <- liftA fromJust $ insertTransaction nodeA 4 4 "" "" "" "" "" "" (Timestamp 405)
                                                                        
        t15 <- liftA fromJust $ insertTransaction nodeB 5 1 "" "" "" "" "" "" (Timestamp 210)
        t16 <- liftA fromJust $ insertTransaction nodeB 5 2 "" "" "" "" "" "" (Timestamp 390)

        addToTransactionRange t1 nodeB
        addToTransactionRange t1 nodeC

        addToTransactionRange t2 nodeB
        addToTransactionRange t2 nodeC

        addToTransactionRange t3 nodeB
        addToTransactionRange t3 nodeC

        addToTransactionRange t5 nodeA
        addToTransactionRange t5 nodeC
        addToTransactionRange t5 nodeD

        addToTransactionRange t6 nodeA
        addToTransactionRange t6 nodeC
        addToTransactionRange t6 nodeD

        -----------------------------------------------

        rawExecute "DELETE FROM range"       [ ]
        rawExecute "DELETE FROM transaction" [ ]
        rawExecute "DELETE FROM device"      [ ]
        rawExecute "DELETE FROM target"      [ ]
        rawExecute "DELETE FROM node"        [ ]

        insertNode $ NewNode "root" Device (Just $ makePwd "root" salt) False

        return ()


connectionStr = C8.pack $ unwords [ key ++ "=" ++ val | (key, val) <- testOpts ]

testOpts :: [(String, String)]
testOpts = 
    [ ("host"     , "localhost")
    , ("user"     , "antenna")
    , ("password" , "antenna")
    , ("dbname"   , "antenna_tests") ]

