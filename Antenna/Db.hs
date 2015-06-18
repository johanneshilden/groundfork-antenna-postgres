module Antenna.Db where

import Control.Monad.Trans.Resource
import Database.Persist.Postgresql
import Control.Monad.Logger

inIO :: ResourceT (NoLoggingT IO) a -> IO a
inIO = runNoLoggingT . runResourceT

runDb :: ConnectionPool -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb pool = inIO . flip runSqlPool pool 

