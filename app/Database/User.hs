{-# LANGUAGE OverloadedStrings #-}

module Database.User (insertUser, getUserById, User) where

import Database.SQLite.Simple

data User = User {
    userId :: Int,
    score :: Int,
    streak :: Int
}

instance FromRow User where
  fromRow = do
    userId <- field
    score <- field
    streak <- field
    return (User userId score streak)

insertUser :: Connection -> User -> IO ()
insertUser conn (User id score streak) =
  execute conn
    "INSERT INTO users (id, score, streak) VALUES (?, ?, ?)"
      (id, score, streak)

getUserById :: Connection -> Int -> IO (Maybe User)
getUserById conn id = 
  do
    res <- query conn "SELECT id, score, streak FROM users WHERE id = ?" (Only id)
    case res of
      [user] -> return (Just user)
      _      -> return Nothing

updateUser :: Connection -> User -> IO ()
updateUser conn (User id score streak) =
  execute conn
    "UPDATE users SET score = ?, streak = ? WHERE id = ?"
      (score, streak, id)
