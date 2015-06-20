{-# LANGUAGE OverloadedStrings #-}
module Antenna.Tests 
    ( runTests
    ) where

import Antenna.Db
import Antenna.Db.Schema
import Antenna.Types
import Control.Monad.IO.Class                        ( liftIO )
import Crypto.PasswordStore
import Database.Persist.Postgresql

import qualified Data.ByteString.Char8            as C8

testAssert :: Bool -> String -> String -> IO ()
testAssert True test message = print $ test ++ " Ok!" 
testAssert ____ test message = print message

assert :: Bool -> String -> String -> SqlT ()
assert a b = liftIO . testAssert a b 

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

        insertDevice "alice" "xxx" salt
        insertDevice "bob" "bob" salt
    
        setNodeTargets "alice" ["bob"]
        setNodeTargets "bob" ["alice"]

        nodeCount <- getNodeCount
        assert (nodeCount == 2) "Test 1" ("Expected getNodeCount == 2, instead got " ++ show nodeCount)

        maybeNode <- getNodeByName "alice"
        assert (_name `fmap` maybeNode == Just "alice") "Test 2" ("Expected (Just 'alice'), instead got " ++ show (_name `fmap` maybeNode))
        assert (_targets `fmap` maybeNode == Just ["bob"]) "Test 3" ("Expected (Just '[\"bob\"]'), instead got " ++ show (_targets `fmap` maybeNode))

        hasd <- hasDevice "alice" "xxx" salt 
        assert hasd "Test 4" "Expected hasDevice 'alice' 'xxx' == True, instead got False"

        hasd <- hasDevice "alice" "pogostick" salt 
        assert (not hasd) "Test 5" "Expected hasDevice 'alice' 'pogostick' == False, instead got True"

        hasd <- hasDevice "what" "not" salt 
        assert (not hasd) "Test 6" "Expected hasDevice 'what' 'not' == False, instead got True"

        deleteNode "bob"
        nodeCount <- getNodeCount
        assert (nodeCount == 1) "Test 7" ("Expected getNodeCount == 1, instead got " ++ show nodeCount)

        hasd <- hasDevice "bob" "bob" salt 
        assert (not hasd) "Test 8" "Expected hasDevice 'bob' 'bob' == False, instead got True"

        insertDevice "bob" "bob" salt

        return ()

  where
    connectionStr = C8.pack $ unwords [ key ++ "=" ++ val | (key, val) <- testOpts ]

    testOpts :: [(String, String)]
    testOpts = 
        [ ("host"     , "localhost")
        , ("user"     , "antenna")
        , ("password" , "antenna")
        , ("dbname"   , "antenna_tests") ]

