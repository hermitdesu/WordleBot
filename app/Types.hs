module Types (Action(..), Model(..), GameState(..), DifficultyLevel(..), Language(..)) where

import Data.Text (Text)


-- some types and synonyms to declare model

type TargetWord = Text

type AmountOfTries = Int

type ListOfGuessed = [Text]

data DifficultyLevel
  = Easy
  | Medium
  | Hard
  deriving (Show, Eq)

data Language
  = Ru
  | En
  deriving (Show)

data GameState
  = Sleep
  | InGame TargetWord AmountOfTries ListOfGuessed
  | SelectingDifficulty
  deriving (Show)

data Model = Model
  { gameState :: GameState
  , currentDifficulty :: DifficultyLevel
  , language :: Language
  } deriving (Show)

-- Action enumeration for telegram bot state

data Action
  = Start
  | Help
  | StartGame
  | StopGame
  | StartGameWithWord Text
  | TextMessage Text
  | DifficultyLevel
  | SetLanguage Language
  | SelectDifficulty DifficultyLevel
  deriving (Show)