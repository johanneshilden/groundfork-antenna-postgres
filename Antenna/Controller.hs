{-# LANGUAGE OverloadedStrings #-}
module Antenna.Controller 
    ( controller
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
import Data.Text                                     ( Text )
import Data.Text.Encoding
import Data.Traversable                              ( sequence )
import Database.Persist.Postgresql                   ( ConnectionPool )
import Network.HTTP.Types
import Network.Wai.Middleware.HttpAuth
import Web.Frank
import Web.Simple

import Prelude                                hiding ( sequence )

respondWith :: ToJSON a => Status -> a -> AppController ()
respondWith status object = respond (responseLBS status headers body)
  where
    headers = [("Content-type", "application/json")]
    body = encode object

data ErrorResponse = JsonError Text
    deriving (Show)

instance ToJSON ErrorResponse where
    toJSON (JsonError code) = object
        [ ("status", "error")
        , ("error", String code) ]

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

    get "ping" $ 
        respond (responseLBS status200 [] "Pong!")

    get "stack" $ authenticate $ 
        \node -> do
            state <- controllerState
            nodes <- liftIO $ runDb (state ^. sqlPool) getNodes
            respondWith status200 nodes

