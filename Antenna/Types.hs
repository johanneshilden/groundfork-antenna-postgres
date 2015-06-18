{-# LANGUAGE TemplateHaskell #-}
module Antenna.Types where

import Control.Lens
import Data.Text                                     ( Text )
import Database.Persist.Sql
import Web.Simple

data Node = Node
    { _nodeId  :: Int
    , _name    :: Text
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

data AppState = AppState { _sqlPool :: ConnectionPool } 

$(makeLenses ''AppState)

type AppController = Controller AppState

