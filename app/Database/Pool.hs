{-# LANGUAGE OverloadedStrings #-}

module Database.Pool (withDbPool, getConn, DbPool) where

import Database.SQLite.Simple (Connection, open, close)
import Data.Pool (Pool, newPool, defaultPoolConfig, withResource)

type DbPool = Pool Connection

withDbPool :: (DbPool -> IO a) -> IO a
withDbPool action = do
  let config = defaultPoolConfig (open "wordle.db") close 1 10
  pool <- newPool config
  action pool

getConn :: DbPool -> (Connection -> IO a) -> IO a
getConn = withResource