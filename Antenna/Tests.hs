{-# LANGUAGE OverloadedStrings #-}
module Antenna.Tests 
    ( runTests
    ) where

import Antenna.Db
import Antenna.Db.Schema
import Antenna.Types
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

        insertNode $ NewNode "alice" Device $ Just (makePwd "xxx" salt)
        insertNode $ NewNode "bob"   Device $ Just (makePwd "bob" salt)
    
        setNodeTargets "alice" ["bob"]
        setNodeTargets "bob" ["alice"]

        nodeCount <- getNodeCount
        assert (nodeCount == 2) "Test 1" ("Expected getNodeCount == 2, instead got " ++ show nodeCount)

        maybeNode <- getNodeByName "alice"
        assert (_name `fmap` maybeNode == Just "alice") "Test 2" ("Expected (Just 'alice'), instead got " ++ show (_name `fmap` maybeNode))
        assert (_targets `fmap` maybeNode == Just ["bob"]) "Test 3" ("Expected (Just '[\"bob\"]'), instead got " ++ show (_targets `fmap` maybeNode))

        hasd <- hasDevice "alice" (makePwd "xxx" salt) 
        assert hasd "Test 4" "Expected hasDevice 'alice' 'xxx' == True, instead got False"

        hasd <- hasDevice "alice" (makePwd "pogostick" salt) 
        assert (not hasd) "Test 5" "Expected hasDevice 'alice' 'pogostick' == False, instead got True"

        hasd <- hasDevice "what" (makePwd "not" salt) 
        assert (not hasd) "Test 6" "Expected hasDevice 'what' 'not' == False, instead got True"

        deleteNode "bob"
        nodeCount <- getNodeCount
        assert (nodeCount == 1) "Test 7" ("Expected getNodeCount == 1, instead got " ++ show nodeCount)

        hasd <- hasDevice "bob" (makePwd "bob" salt) 
        assert (not hasd) "Test 8" "Expected hasDevice 'bob' 'bob' == False, instead got True"

        insertNode $ NewNode "bob" Device $ Just (makePwd "bob" salt)

        updateNode "bob" $ UpdateNode (Just "rob") Nothing Nothing

        hasd <- hasDevice "bob" (makePwd "bob" salt) 
        assert (not hasd) "Test 9" "Expected hasDevice 'bob' 'bob' == False, instead got True"

        hasd <- hasDevice "rob" (makePwd "bob" salt) 
        assert hasd "Test 10" "Expected hasDevice 'rob' 'bob' == True, instead got False"

        insertNode $ NewNode "node3" Virtual Nothing
        insertNode $ NewNode "node4" Virtual Nothing
        insertNode $ NewNode "node5" Virtual Nothing
        insertNode $ NewNode "node6" Virtual Nothing
 
        nodeCount <- getNodeCount
        assert (nodeCount == 6) "Test 11" ("Expected getNodeCount == 11, instead got " ++ show nodeCount)

        updateNode "rob" $ UpdateNode Nothing Nothing (Just ["alice", "node5"])

        maybeNode <- getNodeByName "rob"
        assert (isJust maybeNode) "Test 12" "Expected True == isJust maybeNode, instead got False"

        let node = fromJust maybeNode
        assert (allElems ["alice", "node5"] $ _targets node) "Test 13" "Target list does not match assigned targets."

        updateNode "rob" $ UpdateNode Nothing Nothing (Just ["node5", "node4", "node3", "alice"])

        maybeNode <- getNodeByName "rob"
        assert (isJust maybeNode) "Test 14" "Expected True == isJust maybeNode, instead got False"

        let node = fromJust maybeNode
        assert (allElems ["node5", "node4", "node3", "alice"] $ _targets node) "Test 15" "Target list does not match assigned targets."

        updateNode "rob" $ UpdateNode Nothing Nothing Nothing

        maybeNode <- getNodeByName "rob"
        let node = fromJust maybeNode
        assert (allElems ["node5", "node4", "node3", "alice"] $ _targets node) "Test 16" "Target list does not match assigned targets."

        updateNode "rob" $ UpdateNode Nothing (Just (makePwd "newpwd" salt)) Nothing

        hasd <- hasDevice "rob" (makePwd "bob" salt) 
        assert (not hasd) "Test 17" "Expected hasDevice 'rob' 'bob' == False, instead got True"

        hasd <- hasDevice "rob" (makePwd "newpwd" salt) 
        assert hasd "Test 18" "Expected hasDevice 'rob' 'newpwd' == True, instead got False"

        updateNode "rob" $ UpdateNode Nothing (Just (makePwd "xxx" salt)) (Just [])

        hasd <- hasDevice "rob" (makePwd "xxx" salt) 
        assert hasd "Test 19" "Expected hasDevice 'rob' 'xxx' == True, instead got False"

        maybeNode <- getNodeByName "rob"
        let node = fromJust maybeNode
        assert (allElems [] $ _targets node) "Test 20" "Target list does not match assigned targets."


        return ()

  where
    connectionStr = C8.pack $ unwords [ key ++ "=" ++ val | (key, val) <- testOpts ]

    testOpts :: [(String, String)]
    testOpts = 
        [ ("host"     , "localhost")
        , ("user"     , "antenna")
        , ("password" , "antenna")
        , ("dbname"   , "antenna_tests") ]

