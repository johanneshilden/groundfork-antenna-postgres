module Antenna.Db 
    ( inIO
    , runDb
    , makePwd
    ) where

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Crypto.PasswordStore
import Data.Text                                     ( Text )
import Data.Text.Encoding
import Database.Persist.Postgresql

inIO :: ResourceT (NoLoggingT IO) a -> IO a
inIO = runNoLoggingT . runResourceT

runDb :: ConnectionPool -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb pool = inIO . flip runSqlPool pool 

makePwd :: Text -> Salt -> Text
makePwd pass salt = decodeUtf8 $ makePasswordSalt (encodeUtf8 pass) salt 17

