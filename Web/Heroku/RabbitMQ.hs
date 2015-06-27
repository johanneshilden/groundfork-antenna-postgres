{-# LANGUAGE RecordWildCards #-}
module Web.Heroku.RabbitMQ 
  ( module Network.AMQP
  , AmqpSettings(..)
  , amqpConnSettings
  , openAmqpConnection
  ) where

import Control.Monad
import Data.List.Split
import Data.Text
import Network.AMQP
import System.Environment

data AmqpSettings = AmqpSettings
    { amqpHostName    :: String
    , amqpVirtualHost :: String
    , amqpUser        :: String
    , amqpPass        :: String
    , amqpPort        :: Int 
    } deriving (Show)

amqpConnSettings :: IO AmqpSettings
amqpConnSettings = liftM (parse . splitOneOf "@:/" . trimProtocol) (getEnv "RABBITMQ_BIGWIG_URL")
  where
    parse :: [String] -> AmqpSettings
    parse [ user
          , pass
          , host
          , port
          , vhst 
          ] = AmqpSettings host vhst user pass (read port)
    parse _ = error "Unexpected environment variable format."

    trimProtocol :: String -> String
    trimProtocol ('a':'m':'q':'p':':':'/':'/':rest) = rest
    trimProtocol str = str

openAmqpConnection :: IO Connection
openAmqpConnection = 
     amqpConnSettings >>= \AmqpSettings{..} -> 
        openConnection' 
            amqpHostName 
            (fromIntegral amqpPort)
            (pack amqpVirtualHost)
            (pack amqpUser) 
            (pack amqpPass)

