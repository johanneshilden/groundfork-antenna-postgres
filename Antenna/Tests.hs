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

        insertDevice "alice" "alice" salt
        insertDevice "bob" "bob" salt
    
        replaceNodeTargets "alice" ["bob"]
        replaceNodeTargets "bob" ["alice"]

        nodeCount <- getNodeCount
        assert (nodeCount == 2) "Test 1" ("Expected getNodeCount == 2, instead got " ++ show nodeCount)

        maybeNode <- getNodeByName "alice"
        liftIO $ print maybeNode

        nodes <- getNodes
        liftIO $ print nodes

  where
    connectionStr = C8.pack $ unwords [ key ++ "=" ++ val | (key, val) <- testOpts ]

    testOpts :: [(String, String)]
    testOpts = 
        [ ("host"     , "localhost")
        , ("user"     , "antenna")
        , ("password" , "antenna")
        , ("dbname"   , "antenna_tests") ]

