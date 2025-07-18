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
  , dbPool :: DbPool
  }

instance Show Model where
  show (Model gs _) = "Model { gameState = " ++ show gs ++ ", dbPool = <pool> }"


data Action
  = Start Telegram.Updates
  | Help
  | StartGame
  | StopGame
  | StartGameWithWord Text
  | TextMessage Text
  deriving (Show)