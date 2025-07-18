{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Handlers (handleAction, handleUpdate) where

import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple.UpdateParser 
import Telegram.Bot.Simple         
import Data.Text as T                         (pack, Text, toLower, unpack, concat, intercalate)
import Control.Applicative
import Control.Monad.IO.Class                 (liftIO)

import WordList                               (getRandomWord, isInList, getRandomWordForDifficulty, isInListForDifficulty)
import Messages                               (startMessage, helpMessage, stopGameMessage, stopGameFailMessage, startGameMessage, notInListMessage, gameWonMessage, triesLeftMessage, triesLeftMessage, noMoreTriesMessage, difficultyLevelMessage, selectDifficultyMessage, difficultySelectedMessage)
import Types                                  (Model(..), Action(..), GameState(..), GameState(InGame, Sleep, SelectingDifficulty), DifficultyLevel(..))
import Database.Pool

import qualified Database.User as DB
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)

-- Функция для получения количества попыток в зависимости от уровня сложности
getAttemptsForDifficulty :: DifficultyLevel -> Int
getAttemptsForDifficulty Easy = 6
getAttemptsForDifficulty Medium = 5
getAttemptsForDifficulty Hard = 4

handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate model =
  parseUpdate $
      Start <$ command (pack "start")
      <|> Help <$ command (pack "help")
      <|> StartGame <$ command (pack "game")
      <|> StopGame <$ command (pack "stop")
      <|> DifficultyLevel <$ command (pack "difficulty_level")
      <|> (parseDifficultySelection <$> plainText)
  where
    parseDifficultySelection :: Text -> Action
    parseDifficultySelection text
      | text == pack "Easy" = SelectDifficulty Easy
      | text == pack "Medium" = SelectDifficulty Medium
      | text == pack "Hard" = SelectDifficulty Hard
      | otherwise = TextMessage text

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = 
  case action of
    Start -> handleStart model
    Help -> handleHelp model
    StartGame -> handleStartGame model
    StopGame -> handleStopGame model
    StartGameWithWord word -> 
        let attempts = getAttemptsForDifficulty (currentDifficulty model)
            newModel = model { gameState = InGame word attempts []}
        in newModel <# replyText startGameMessage
    TextMessage word -> handleGuess model word
    DifficultyLevel -> handleDifficultyLevel model
    SelectDifficulty diff -> handleSelectDifficulty diff model

handleStart :: Model -> Eff Action Model
handleStart model = model <# do
  reply $
    (toReplyMessage startMessage)
      { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup wordleKeyboard) }

handleHelp :: Model -> Eff Action Model
handleHelp model =
  model <# do
    replyText helpMessage

handleStartGame :: Model -> Eff Action Model
handleStartGame model = 
  model <# liftIO (getRandomWordForDifficulty (currentDifficulty model) >>= return . StartGameWithWord)

handleDifficultyLevel :: Model -> Eff Action Model
handleDifficultyLevel model =
  model <# do
    reply $
      (toReplyMessage selectDifficultyMessage)
        { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup difficultyKeyboard) }

handleSelectDifficulty :: DifficultyLevel -> Model -> Eff Action Model
handleSelectDifficulty diff model =
  let newModel = model { currentDifficulty = diff, gameState = Sleep }
  in newModel <# do
      reply $
        (toReplyMessage (difficultySelectedMessage diff))
          { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup wordleKeyboard) }

handleStopGame :: Model -> Eff Action Model
handleStopGame model =
  case gameState model of
    InGame {} ->
      model {gameState = Sleep} <# do
        reply $
          (toReplyMessage stopGameMessage)
            { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup wordleKeyboard) }
    Sleep ->
      model <# do
        replyText stopGameFailMessage

handleGuess :: Model -> Text -> Eff Action Model
handleGuess model guessedRaw =
  let guessed = T.toLower guessedRaw
  in case gameState model of
    Sleep -> pure model
    InGame target cnt guesslist
      | not (isInListForDifficulty (currentDifficulty model) guessed) ->
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
          reply $
            (toReplyMessage (allFeedback <> gameWonMessage))
              { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup wordleKeyboard) }
        
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
              reply $
                (toReplyMessage (allFeedback <> noMoreTriesMessage <> target))
                  { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup wordleKeyboard) }
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

-- Клавиатура для выбора уровня сложности
difficultyKeyboard :: Telegram.ReplyKeyboardMarkup
difficultyKeyboard =
  Telegram.ReplyKeyboardMarkup
    { Telegram.replyKeyboardMarkupKeyboard =
        [ [ Telegram.KeyboardButton (pack "Easy") Nothing Nothing Nothing Nothing Nothing Nothing
          , Telegram.KeyboardButton (pack "Medium") Nothing Nothing Nothing Nothing Nothing Nothing
          , Telegram.KeyboardButton (pack "Hard") Nothing Nothing Nothing Nothing Nothing Nothing
          ]
        ]
    , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
    , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just True
    , Telegram.replyKeyboardMarkupSelective = Nothing
    , Telegram.replyKeyboardMarkupInputFieldSelector = Nothing
    , Telegram.replyKeyboardMarkupIsPersistent = Nothing
    }

wordleKeyboard :: Telegram.ReplyKeyboardMarkup
wordleKeyboard =
  Telegram.ReplyKeyboardMarkup
    { Telegram.replyKeyboardMarkupKeyboard =
        [ [ Telegram.KeyboardButton (pack "/game") Nothing Nothing Nothing Nothing Nothing Nothing
          , Telegram.KeyboardButton (pack "/help") Nothing Nothing Nothing Nothing Nothing Nothing
          ]
        , [ Telegram.KeyboardButton (pack "/stop") Nothing Nothing Nothing Nothing Nothing Nothing
          , Telegram.KeyboardButton (pack "/difficulty_level") Nothing Nothing Nothing Nothing Nothing Nothing
          ]
        ]
    , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
    , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just True
    , Telegram.replyKeyboardMarkupSelective = Nothing
    , Telegram.replyKeyboardMarkupInputFieldSelector = Nothing
    , Telegram.replyKeyboardMarkupIsPersistent = Nothing
    }