{-# LANGUAGE OverloadedStrings #-}

module Database.Migrations (runMigrations) where

import Database.SQLite.Simple

runMigrations :: Connection -> IO ()
runMigrations conn = do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, username TEXT NOT NULL)"
