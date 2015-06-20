{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Antenna.Types where

import Control.Lens                                  ( makeLenses, (&) )
import Crypto.PasswordStore
import Data.Aeson
import Data.Text                                     ( Text )
import Database.Persist.Sql
import Web.Simple

data NodeType = Device | Virtual
    deriving (Show, Read)

toText :: NodeType -> Text
toText Virtual = "virtual"
toText _______ = "device"

toType :: Text -> NodeType
toType "virtual" = Virtual
toType _________ = Device

data Node = Node
    { _nodeId  :: Int
    , _name    :: Text
    , _family  :: NodeType
    , _targets :: [Text]
    } deriving (Show)

data Transaction = Transaction
    { _transactionId :: Int
    , _upAction      :: Text
    , _downAction    :: Text
    , _timestamp     :: Int
    , _range         :: [Text]
    } deriving (Show)

$(makeLenses ''Node)
$(makeLenses ''Transaction)

data AppState = AppState 
    { _sqlPool :: ConnectionPool 
    , _salt    :: Salt
    } 

$(makeLenses ''AppState)

type AppController = Controller AppState

instance ToJSON Node where
    toJSON node = object
        [ "id"      .= (node & _nodeId)
        , "name"    .= (node & _name) 
        , "type"    .= toText (node & _family) 
        , "targets" .= (node & _targets) 
        ]

