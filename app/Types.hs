module Types (Action(..), Model(..), GameState(..), DifficultyLevel(..)) where

import Data.Text (Text)
import Database.Pool (DbPool)
import Telegram.Bot.API as Telegram

type TargetWord = Text

type AmountOfTries = Int

type ListOfGuessed = [Text]

data DifficultyLevel
  = Easy
  | Medium
  | Hard
  deriving (Show, Eq)

data GameState
  = Sleep
  | InGame TargetWord AmountOfTries ListOfGuessed
  | SelectingDifficulty
  deriving (Show)

data Model = Model
  { gameState :: GameState
  , currentDifficulty :: DifficultyLevel
  } deriving (Show)

data Action
  = Start
  | Help
  | StartGame
  | StopGame
  | StartGameWithWord Text
  | TextMessage Text
  | DifficultyLevel
  | SelectDifficulty DifficultyLevel
  deriving (Show)