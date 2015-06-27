{-# LANGUAGE OverloadedStrings #-}
module Antenna.Controller 
    ( controller
    , okResponse
    ) where

import Antenna.Db                                    
import Antenna.Db.Schema                      hiding ( Node )
import Antenna.Sync
import Antenna.Types
import Control.Applicative
import Control.Lens                                  ( ix, traverse, over, _Just, (^?), (^.), (^..), (<&>) )
import Control.Monad                                 ( join )
import Control.Monad.Trans                           ( liftIO )
import Crypto.PasswordStore                          ( Salt )
import Data.Aeson
import Data.Aeson.Lens
import Data.HashMap.Strict                           ( HashMap, fromList )
import Data.Maybe                                    ( fromMaybe, maybeToList )
import Data.Scientific
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

import qualified Data.Vector                      as Vect

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

lookupDevice :: Text -> Text -> Salt -> SqlT (Maybe Node)
lookupDevice name pass salt = lookupCredentials name secret
  where
    secret = makePwd pass salt

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

    authenticate $ \node -> do

        get "nodes" $ do
            nodes <- runQuery getNodes
            let len = length nodes
                collection = Collection len len (Payload "nodes" nodes)
            respondWith status200 collection

        post "nodes" $ do
            req  <- request
            body <- liftIO $ strictRequestBody req
            case decode body of
              Just (Object o) -> processNewNode o
              _______________ -> respondWith status400 (JsonError "BAD_REQUEST")

        put "nodes/:id" $ do
            node <- liftA (fromMaybe 0) (readQueryParam "id")
            req  <- request
            body <- liftIO $ strictRequestBody req
            case decode body of
              Just (Object o) -> processUpdateNode node o
              _______________ -> respondWith status400 (JsonError "BAD_REQUEST")

        delete "nodes/:id" $ do
            node <- liftA (fromMaybe 0) (readQueryParam "id")
            let nodeId = toKey node
            runQuery (deleteNode nodeId)
            respondWith status200 (JsonOk Nothing)

        post "sync" $ do
            req  <- request
            body <- liftIO $ strictRequestBody req
            case decode body of
              Just o -> processSyncRequest node o
              ______ -> respondWith status400 (JsonError "BAD_REQUEST")

        get "log" $ do
            page <- liftA (numeric  1) (queryParam "page")
            size <- liftA (numeric 25) (queryParam "size")
            let offs = (page - 1) * size
            transactions <- runQuery $ getTransactionsPage offs size
            total <- runQuery getTransactionCount 
            let collection = Collection (length transactions) total (Payload "transactions" transactions)
            respondWith status200 collection

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
        nodeName <- object ^? ix "name"._String
        nodeType <- object ^? ix "type"._String
        let locked = fromMaybe False (object ^? ix "locked"._Bool)
        case nodeType of
          "device" -> do
            pass <- object ^? ix "password"._String
            let secret = makePwd pass salt
            return $ NewNode nodeName Device (Just secret) locked
          "virtual" -> 
            return $ NewNode nodeName Virtual Nothing locked 

processUpdateNode :: Int -> HashMap Text Value -> AppController ()
processUpdateNode nodeId object = do
    app <- controllerState
    let nodeTargets = over (_Just . traverse) (^.._Number) targets <&> join . Vect.toList
        node = UpdateNode nodeName nodePass (toInt nodeTargets)
    liftIO $ print nodeTargets
    liftIO $ print (toInt nodeTargets)
    response <- liftIO . runDb (app ^. sqlPool) $ updateNode (toKey nodeId) node
    case response of
      UpdateNotFound -> respondWith status404 (JsonError "NOT_FOUND")
      UpdateSuccess  -> respondWith status200 (JsonOk Nothing)
  where
    toInt = (fmap . fmap) (fromIntegral . coefficient)
    nodeName = object ^? ix "name"     ._String
    nodePass = object ^? ix "password" ._String 
    targets  = object ^? ix "targets"  ._Array 

