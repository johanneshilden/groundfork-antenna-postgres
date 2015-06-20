{-# LANGUAGE OverloadedStrings #-}
module Antenna.Controller 
    ( controller
    , okResponse
    ) where

import Antenna.Db                                    ( runDb )
import Antenna.Db.Schema
import Antenna.Types
import Control.Applicative
import Control.Lens
import Control.Monad                                 ( join )
import Control.Monad.Trans                           ( liftIO )
import Crypto.PasswordStore
import Data.Aeson
import Data.Aeson.Lens
import Data.HashMap.Strict                           ( HashMap, fromList )
import Data.Maybe                                    ( fromMaybe, maybeToList )
import Data.Text                                     ( Text )
import Data.Text.Encoding
import Data.Traversable                              ( sequence )
import Database.Persist.Postgresql                   ( ConnectionPool )
import Network.HTTP.Types
import Network.Wai.Middleware.HttpAuth
import Text.Read                                     ( readMaybe )
import Web.Frank
import Web.Simple

import Prelude                                hiding ( sequence )

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
        [ ("status"  , "success")
        , ("message" , "OK") 
        ] ++ maybeToList (msgBody <$> body)
      where
        msgBody body = ("body", body)

instance ToJSON ErrorResponse where
    toJSON (JsonError code) = object
        [ ("status" , "error")
        , ("error"  , String code) ]

okResponse :: Object -> OkResponse
okResponse = JsonOk . Just . Object  

authenticate :: (Node -> AppController ()) -> AppController ()
authenticate action = do
    app <- controllerState
    req <- request
    maybeNode <- join <$> liftIO (nodeCredentials app req)
    case maybeNode of
      Just node -> action node
      Nothing -> respondWith status401 (JsonError "UNAUTHORIZED")
  where 
    nodeCredentials app request = sequence $ lookupNode 
        <$> credentials request 
        <*> Just (app ^. sqlPool) 
        <*> Just (app ^. salt)

lookupNode :: (Text, Text) -> ConnectionPool -> Salt -> IO (Maybe Node)
lookupNode (name, secret) pool = runDb pool . lookupDevice name secret 

credentials :: Request -> Maybe (Text, Text)
credentials request = do
    headers <- lookup "Authorization" (requestHeaders request)
    (name, pass) <- extractBasicAuth headers 
    return (decodeUtf8 name, decodeUtf8 pass) 

controller :: AppController ()
controller = do
    state <- controllerState
    let runQuery = liftIO . runDb (state ^. sqlPool)

    get "ping" $ respond (responseLBS status200 [] "Pong!")

    authenticate $ \_ -> do

        get "nodes" $ do
            nodes <- runQuery getNodes
            respondWith status200 nodes

        post "nodes" $ do
            req  <- request
            body <- liftIO $ strictRequestBody req
            case decode body of
              Just (Object o) -> processNewNode o
              _______________ -> respondWith status400 (JsonError "BAD_REQUEST")

        put "nodes/:name" $ do
            --name <- liftA (fromMaybe "") (queryParam "name")
            req  <- request
            body <- liftIO $ strictRequestBody req
            undefined

        delete "nodes/:name" $ do
            name <- liftA (fromMaybe "") (queryParam "name")
            runQuery (deleteNode name)
            respondWith status200 (JsonOk Nothing)

        post "sync" $ do
            req  <- request
            body <- liftIO $ strictRequestBody req
            undefined

        get "log" $ do
            page <- liftA (numeric  1) (queryParam "page")
            size <- liftA (numeric 25) (queryParam "size")
            let offs = (page - 1) * size
            transactions <- runQuery $ getTransactionsPage offs size
            respondWith status200 transactions

        post "log/reset" $ do
            runQuery deleteAllTransactions
            respondWith status200 (JsonOk Nothing)

    respondWith status404 (JsonError "NOT_FOUND")

numeric :: Int -> Maybe String -> Int
numeric def = fromMaybe def . join . fmap readMaybe 

processNewNode :: HashMap Text Value -> AppController ()
processNewNode object = do
    app <- controllerState
    case buildNode (app ^. salt) of
      Nothing -> respondWith status400 (JsonError "BAD_REQUEST")
      Just newNode -> do
        response <- liftIO . runDb (app ^. sqlPool) $ insertNode newNode
        case response of
          InsertSuccess key -> respondWith status200 (okResponse $ idResponse key)
          InsertConflict    -> respondWith status409 (JsonError "CONFLICT")
          InsertBadRequest  -> respondWith status400 (JsonError "BAD_REQUEST")
  where
    idResponse key = fromList [("id", Number $ fromIntegral $ unKey key)]
    buildNode salt = do
        nodeName <- object ^? ix "name" ._String
        nodeType <- object ^? ix "type" ._String
        case nodeType of
          "device" -> do
            pass <- object ^? ix "device" ._String
            return $ NewNode nodeName Device $ Just (pass, salt)
          "virtual" -> 
            return $ NewNode nodeName Virtual Nothing

