{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Handlers (handleAction, handleUpdate) where

import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple.UpdateParser 
import Telegram.Bot.Simple         
import Data.Text as T                         (pack)
import Control.Applicative
import Control.Monad.IO.Class                 (liftIO)

import WordList                               (getRandomWord)
import Messages                               (startMessage, helpMessage, stopGameMessage, stopGameFailMessage, startGameMessage)
import Types                                  (Model(..), Action(..), GameState(..), GameState(InGame, Sleep))


gameRounds :: Int
gameRounds = 5

handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ =
  parseUpdate $
  Start <$ command (pack "start")
      <|> Help <$ command (pack "help")
      <|> StartGame <$ command (pack "game")
      <|> StopGame <$ command (pack "stop")

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = 
  case action of
    Start -> handleStart model
    Help -> handleHelp model
    StartGame -> handleStartGame model
    StopGame -> handleStopGame model
    StartGameWithWord word -> 
        let newModel = model { gameState = InGame word gameRounds }
        in newModel <# replyText startGameMessage


handleStart :: Model -> Eff Action Model
handleStart model =
  model <# do
    reply $
      (toReplyMessage startMessage)
        {replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup wordleKeyboard)}

handleHelp :: Model -> Eff Action Model
handleHelp model =
  model <# do
    replyText helpMessage

handleStartGame :: Model -> Eff Action Model
handleStartGame model = 
  model <# liftIO (StartGameWithWord <$> getRandomWord)


handleStopGame :: Model -> Eff Action Model
handleStopGame model =
  case gameState model of
    InGame _ _->
      model {gameState = Sleep} <# do
        replyText stopGameMessage
    Sleep ->
      model <# do
        replyText stopGameFailMessage

wordleKeyboard :: Telegram.ReplyKeyboardMarkup
wordleKeyboard =
  Telegram.ReplyKeyboardMarkup
    { Telegram.replyKeyboardMarkupKeyboard =
        [ [ Telegram.KeyboardButton (pack "/game") Nothing Nothing Nothing Nothing Nothing Nothing
          , Telegram.KeyboardButton (pack "/help") Nothing Nothing Nothing Nothing Nothing Nothing
          ]
        ]
    , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
    , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just True
    , Telegram.replyKeyboardMarkupSelective = Nothing
    , Telegram.replyKeyboardMarkupInputFieldSelector = Nothing
    , Telegram.replyKeyboardMarkupIsPersistent = Nothing
    }