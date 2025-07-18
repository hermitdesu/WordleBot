module Types (Action(..), Model(..), GameState(..)) where

import Data.Text (Text)
import Database.Pool (DbPool)
import Telegram.Bot.API as Telegram

type TargetWord = Text

type AmountOfTries = Int

type ListOfGuessed = [Text]

data GameState
  = Sleep
  | InGame TargetWord AmountOfTries ListOfGuessed
  deriving (Show)

data Model = Model
  { gameState :: GameState
  } deriving (Show)

data Action
  = Start
  | Help
  | StartGame
  | StopGame
  | StartGameWithWord Text
  | TextMessage Text
  deriving (Show)