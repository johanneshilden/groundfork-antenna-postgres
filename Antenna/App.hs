module Antenna.App where

import Antenna.Controller
import Antenna.Types
import Web.Simple

waiApp :: AppState -> Application
waiApp state = controllerApp state controller 

