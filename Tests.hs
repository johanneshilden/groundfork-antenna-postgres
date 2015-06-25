{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Exception.Base
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Char
import Data.String.Utils
import Data.Maybe                        ( fromJust, catMaybes )
import Data.Scientific
import Data.Text                         ( Text )
import Network.HTTP.Client
import Network.HTTP.Types

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Search  as Search
import qualified Data.ByteString.Char8   as C8
import qualified Data.ByteString.Lazy    as BL
import qualified Data.HashMap.Strict     as MapS
import qualified Data.Vector             as Vect

check :: Status -> ResponseHeaders -> CookieJar -> Maybe SomeException
check _ _ _ = Nothing

testAssert :: Bool -> String -> String -> IO ()
testAssert True  test _   = print $ test ++ " Ok."
testAssert False test msg = error $ show $ test ++ ": " ++ msg

extractPayload :: BS.ByteString -> Maybe (MapS.HashMap Text Value)
extractPayload body = do        
    b <- decode (BL.fromStrict body)
    o <- takeObject b
    MapS.lookup "body" o >>= takeObject 
  where
    takeObject (Object o) = Just o
    takeObject _ = Nothing

main :: IO ()
main = do

    manager <- newManager defaultManagerSettings

    -------------------------------------------------------------

    initReq <- parseUrl "http://localhost:3333/ping"
    let req = initReq { method = "GET" }

    withResponse req manager $ \response -> do
        let bodyReader = responseBody response
        body <- bodyReader
        testAssert (body == "Pong!") "Test 1" $ "Expected response body 'Pong!', instead got '" ++ show body ++ "'"

    print "-------------------------------------------------------------"

    -------------------------------------------------------------
    
    let body = Object $ MapS.fromList 
            [ ("name"     , String "test")
            , ("type"     , String "device")
            , ("locked"   , Bool True)
            , ("password" , String "hello") ]

    initReq <- parseUrl "http://localhost:3333/nodes"
    let req = initReq 
                { method = "POST" 
                , requestBody = RequestBodyLBS $ encode body 
                , checkStatus = check
                }
    let req' = applyBasicAuth "root" "root" req

    node1Id <- withResponse req' manager $ \response -> do
        let bodyReader = responseBody response
        body <- bodyReader
        let Object obj = fromJust $ decode (BL.fromStrict body)
        testAssert (MapS.lookup "status" obj == Just (String "success")) "Test 2" "Expected response object having key status == 'success'."

        case extractPayload body of
          Nothing -> error "Test 2.1: Unexpected response."
          Just obj -> do
            let Number n = fromJust $ MapS.lookup "id" obj
            return $ coefficient n

    print "-------------------------------------------------------------"

    -------------------------------------------------------------

    let body = Object $ MapS.fromList 
            [ ("name"     , String "test-2")
            , ("type"     , String "device")
            , ("locked"   , Bool True)
            , ("password" , String "hello") ]

    initReq <- parseUrl "http://localhost:3333/nodes"
    let req = initReq 
                { method = "POST" 
                , requestBody = RequestBodyLBS $ encode body 
                , checkStatus = check
                }
    let req' = applyBasicAuth "root" "root" req

    node2Id <- withResponse req' manager $ \response -> do
        let bodyReader = responseBody response
        body <- bodyReader
        case extractPayload body of
          Nothing -> error "Test 2.1: Unexpected response."
          Just obj -> do
            let Number n = fromJust $ MapS.lookup "id" obj
            return $ coefficient n


    print "-------------------------------------------------------------"

    -------------------------------------------------------------
    
    let body = Object $ MapS.fromList 
            [ ("name"     , String "test-3")
            , ("type"     , String "device")
            , ("locked"   , Bool True)
            , ("password" , String "hello") ]

    initReq <- parseUrl "http://localhost:3333/nodes"
    let req = initReq 
                { method = "POST" 
                , requestBody = RequestBodyLBS $ encode body 
                , checkStatus = check
                }
    let req' = applyBasicAuth "root" "root" req

    node3Id <- withResponse req' manager $ \response -> do
        let bodyReader = responseBody response
        body <- bodyReader
        case extractPayload body of
          Nothing -> error "Test 2.1: Unexpected response."
          Just obj -> do
            let Number n = fromJust $ MapS.lookup "id" obj
            return $ coefficient n


    print "-------------------------------------------------------------"

    -------------------------------------------------------------

    let body = Object $ MapS.fromList 
            [ ("name"     , String "test")
            , ("type"     , String "device")
            , ("locked"   , Bool True)
            , ("password" , String "hello") ]

    initReq <- parseUrl "http://localhost:3333/nodes"
    let req = initReq 
                { method      = "POST" 
                , requestBody = RequestBodyLBS $ encode body 
                , checkStatus = check
                }
    let req' = applyBasicAuth "root" "root" req

    withResponse req' manager $ \response -> do
        let bodyReader = responseBody response
        let respStatus = responseStatus response
        body <- bodyReader
        let Object obj = fromJust $ decode (BL.fromStrict body)
        testAssert (MapS.lookup "error" obj == Just (String "CONFLICT")) "Test 3" $ "Expected response object having key error == 'CONFLICT'."
        testAssert (respStatus == status409) "Test 4" $ "Expected response status 409, instead got " ++ show respStatus


    print "-------------------------------------------------------------"
    
    -------------------------------------------------------------

    let syncReq = Object $ MapS.fromList 
            [ ("targets"   , Array (Vect.fromList ["test-1", "test-2", "test-3", "test-4"]))
            , ("syncPoint" , Number 0)
            , ("commit"    , Array (Vect.fromList []))
            ]

    initReq <- parseUrl "http://localhost:3333/sync"
    let req = initReq 
                { method = "POST" 
                , requestBody = RequestBodyLBS $ encode syncReq 
                , checkStatus = check 
                }
    let req' = applyBasicAuth "test" "hello" req

    withResponse req' manager $ \response -> do
        let bodyReader = responseBody response
        let respStatus = responseStatus response
        body <- bodyReader
        let Object obj = fromJust $ decode (BL.fromStrict body)
        testAssert (MapS.lookup "status" obj == Just (String "success")) "Test 5" "Expected response object having key status == 'success'."


    print "-------------------------------------------------------------"
    
    -------------------------------------------------------------

    let syncReq = Object $ MapS.fromList 
            [ ("targets"   , Array (Vect.fromList ["test-1", "test-2", "test-3", "test-4"]))
            , ("syncPoint" , Number 0)
            , ("commit"    , Array (Vect.fromList []))
            ]

    initReq <- parseUrl "http://localhost:3333/sync"
    let req = initReq 
                { method = "POST" 
                , requestBody = RequestBodyLBS $ encode syncReq 
                , checkStatus = check 
                }
    let req' = applyBasicAuth "test" "hello" req

    withResponse req' manager $ \response -> do
        let bodyReader = responseBody response
        let respStatus = responseStatus response
        body <- bodyReader
        let Object obj = fromJust $ decode (BL.fromStrict body)
        testAssert (MapS.lookup "status" obj == Just (String "success")) "Test 6" "Expected response object having key status == 'success'."


    print "-------------------------------------------------------------"

    -------------------------------------------------------------
    
    let customer_1 = Object $ MapS.fromList 
            [ ("up", Object $ MapS.fromList 
                    [ ("method", String "POST")
                    , ("resource", String "customers")
                    , ("payload", mkTestObject "customer-1" "||customers/1||") ]) 
            , ("down", Object $ MapS.fromList 
                    [ ("method", String "DELETE")
                    , ("resource", String "||customers/1||") ]) 
            , ("timestamp", Number 11110)
            , ("index", Number 1)
            ]

    r <- runSync 
        manager
        ["test-2", "test-3", "test-4"]
        0
        [ customer_1 ]
        "test" "hello"

    case decode (BL.fromStrict r) of
        Just (Object o) -> do
            let f = MapS.lookup "forward" o
            case f of
              Just (Array v) -> do
                let a = Vect.toList v
                testAssert (length a == 1) "Test 7.1" "Expected response forward array length == 1"
              _ -> error "Test 7. Unexpected response."
        _ -> error "Test 7. Unexpected response."


    -------------------------------------------------------------

    let customer_1 = Object $ MapS.fromList 
             [ ("up", Object $ MapS.fromList 
                     [ ("method", String "POST")
                     , ("resource", String "customers")
                     , ("payload", mkTestObject "customer-1" "||customers/1||") ]) 
             , ("down", Object $ MapS.fromList 
                     [ ("method", String "DELETE")
                     , ("resource", String "||customers/1||") ]) 
             , ("timestamp", Number 9050)
             , ("index", Number 1)
            ]
    let customer_2 = Object $ MapS.fromList 
             [ ("up", Object $ MapS.fromList 
                     [ ("method", String "POST")
                     , ("resource", String "customers")
                     , ("payload", mkTestObject "customer-2" "||customers/2||") ]) 
             , ("down", Object $ MapS.fromList 
                     [ ("method", String "DELETE")
                     , ("resource", String "||customers/2||") ]) 
             , ("timestamp", Number 20050)
             , ("index", Number 2)
             ]
 
    r <- runSync 
         manager
         ["test", "test-3", "test-4"]
         0
         [ customer_1, customer_2 ]
         "test-2" "hello"

 
    let x = fromJust $ decode (BL.fromStrict r) :: MapS.HashMap Text Value
    let s = map (chr . fromEnum) $ BL.unpack $ encodePretty x

     -- check response

    let fwd = let Array a = (fromJust (MapS.lookup "forward" x)) in Vect.toList a
    let rev = let Array a = (fromJust (MapS.lookup "reverse" x)) in Vect.toList a

    let fwd' = catMaybes $ takeHrefs <$> fwd
    let rev' = catMaybes $ takeHrefs <$> rev

    testAssert (fwd' == [String "customers/id_2-1", String "customers/id_1-1", String "customers/id_2-2"]) "Test 8.1" $ "Unexpected result: " ++ show fwd'
    testAssert (rev' == []) "Test 8.2" $ "Unexpected result: " ++ show rev'


    return ()


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

mkTestObject name href = Object $ MapS.fromList
        [ ("name", name)
        , ("_links", Object $ MapS.fromList
                [ ("self", Object $ MapS.fromList
                        [ ("href", String href) ])
                ])
        ]

runSync manager targets syncPoint commit device pass = do

    let syncReq = Object $ MapS.fromList 
            [ ("targets"   , Array (Vect.fromList targets))
            , ("syncPoint" , Number syncPoint)
            , ("commit"    , Array (Vect.fromList commit)) ]

    initReq <- parseUrl "http://localhost:3333/sync"
    let req = initReq 
                { method = "POST" 
                , requestBody = RequestBodyLBS $ encode syncReq 
                , checkStatus = check 
                }
    let req' = applyBasicAuth device pass req

    withResponse req' manager $ \response -> do
        let bodyReader = responseBody response
        body <- bodyReader
        return body

takeHrefs (Object o) = do
    (Object o') <- MapS.lookup "payload" o 
    (Object o'') <- MapS.lookup "_links" o'
    (Object o''') <- MapS.lookup "self" o''
    MapS.lookup "href" o'''

