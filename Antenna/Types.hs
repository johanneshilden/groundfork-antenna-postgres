{-# LANGUAGE TemplateHaskell   #-}
module Antenna.Types 
    ( AppState
    , insertListener
    , removeListener
    , initialState
    ) where

import Control.Lens
import Data.HashMap.Strict                           ( HashMap, insert, delete, empty )
import Data.Text                                     ( Text )
import Network.WebSockets

type ListenerMap = HashMap Text Connection 

data AppState = AppState 
    { _listeners :: ListenerMap 
    } 

$(makeLenses ''AppState)

insertListener :: Text -> Connection -> AppState -> AppState
insertListener node = over listeners . insert node 

removeListener :: Text -> AppState -> AppState
removeListener = over listeners . delete 

initialState :: AppState
initialState = AppState empty

