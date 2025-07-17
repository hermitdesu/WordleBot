module Types (Action(..), Model(..), GameState(..)) where

import Data.Text (Text)

data GameState
  = Sleep
  | InGame Text Int
  deriving (Show)


data Model = Model 
  { 
    gameState :: GameState 
  } deriving (Show)


data Action
  = Start
  | Help
  | StartGame
  | StopGame
  | StartGameWithWord Text
  | TextMessage Text
  deriving (Show)