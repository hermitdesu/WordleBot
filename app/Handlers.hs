{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Handlers (handleAction, handleUpdate) where

import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple.UpdateParser 
import Telegram.Bot.Simple         
import Data.Text as T                         (pack, Text, toLower, unpack, concat, intercalate)
import Control.Applicative
import Control.Monad.IO.Class                 (liftIO)

import WordList                               (getRandomWord, isInList)
import Messages                               (startMessage, helpMessage, stopGameMessage, stopGameFailMessage, startGameMessage, notInListMessage, gameWonMessage, triesLeftMessage, triesLeftMessage, noMoreTriesMessage)
import Types                                  (Model(..), Action(..), GameState(..), GameState(InGame, Sleep))
import Database.Pool

import qualified Database.User as DB
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)


gameRounds :: Int
gameRounds = 5

handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _model update =
  parseUpdate $
  Start update <$ command (pack "start") update
      <|> Help <$ command (pack "help")
      <|> StartGame <$ command (pack "game")
      <|> StopGame <$ command (pack "stop")
      <|> TextMessage <$> plainText

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = 
  case action of
    Start update -> handleStart model
    Help -> handleHelp model
    StartGame -> handleStartGame model
    StopGame -> handleStopGame model
    StartGameWithWord word -> 
        let newModel = model { gameState = InGame word gameRounds []}
        in newModel <# replyText startGameMessage
    TextMessage word -> handleGuess model word


handleStart :: Telegram.Update -> Model -> Eff Action Model
handleStart update model = model <# do
  reply $
    (toReplyMessage startMessage)
      { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup wordleKeyboard) }

  let pool = dbPool model
  case Telegram.updateMessage update >>= Telegram.messageFrom of
    Just user -> do
      let userId = Telegram.unUserId (Telegram.userId user)
      liftIO $ getConn pool $ \conn ->
        DB.insertUser conn (DB.User userId 0 0)
    Nothing -> pure ()

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
    InGame {} ->
      model {gameState = Sleep} <# do
        replyText stopGameMessage
    Sleep ->
      model <# do
        replyText stopGameFailMessage

handleGuess :: Model -> Text -> Eff Action Model
handleGuess model guessedRaw =
  let guessed = T.toLower guessedRaw
  in case gameState model of
    Sleep -> pure model
    InGame target cnt guesslist
      | not (isInList guessed) ->
        model <# do
          replyText notInListMessage
      | guessed == target ->
        let
          newGuesses = guesslist ++ [guessed]
          allFeedback = T.intercalate (pack "\n") $
            zipWith (\guess fb -> fb <> pack " - " <> guess)
                    newGuesses
                    (map (`getColorFeedback` target) newGuesses)
        in model { gameState = Sleep } <# do
          replyText (allFeedback <> gameWonMessage)
        
      | otherwise ->
        let
          newGuesses = guesslist ++ [guessed]
          allFeedback = T.intercalate (pack "\n") $
            zipWith (\guess fb -> fb <> pack " - " <> guess)
                    newGuesses
                    (map (`getColorFeedback` target) newGuesses)
        in
          if cnt <= 1
            then model { gameState = Sleep } <# do
              replyText (allFeedback <> noMoreTriesMessage <> target)
            else model { gameState = InGame target (cnt - 1) newGuesses } <# do
              replyText (allFeedback <> triesLeftMessage <> pack (show (cnt - 1)))

getColorFeedback :: Text -> Text -> Text
getColorFeedback guessed correct =
  let
    greenSticker = pack "🟩"
    yellowSticker = pack "🟨"
    graySticker = pack "⬜"

    greensAndRemainder :: [Char] -> [Char] -> ([Char], [Char])
    greensAndRemainder [] [] = ([], [])
    greensAndRemainder (g:gs) (c:cs)
      | g == c    = let (gsRes, rem) = greensAndRemainder gs cs
                    in ('G':gsRes, rem)
      | otherwise = let (gsRes, rem) = greensAndRemainder gs cs
                    in ('B':gsRes, c:rem)
    greensAndRemainder _ _ = error "Lists must be same length"

    removeFirst :: Eq a => a -> [a] -> [a]
    removeFirst _ [] = []
    removeFirst x (y:ys)
      | x == y    = ys
      | otherwise = y : removeFirst x ys

    yellows :: [Char] -> [Char] -> [Char] -> [Text]
    yellows [] [] _ = []
    yellows (g:gs) (m:ms) rem
      | m == 'G'     = greenSticker : yellows gs ms rem
      | g `elem` rem = yellowSticker : yellows gs ms (removeFirst g rem)
      | otherwise    = graySticker : yellows gs ms rem
    yellows _ _ _ = error "Lists must be same length"

    (greenMarks, remainder) = greensAndRemainder (unpack guessed) (unpack correct)
    feedback = yellows (unpack guessed) greenMarks remainder
  in T.concat feedback


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