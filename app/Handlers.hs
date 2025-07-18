{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Handlers (handleAction, handleUpdate) where

import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple.UpdateParser 
import Telegram.Bot.Simple         
import Data.Text as T                         (pack, Text, toLower, unpack, concat, intercalate)
import Control.Applicative
import Control.Monad.IO.Class                 (liftIO)

import WordList                               (getRandomWordForDifficulty, isInListForDifficulty, isInList)
import Messages                               (startMessage, helpMessage, stopGameMessage, stopGameFailMessage, startGameMessage, notInListMessage, gameWonMessage, triesLeftMessage, triesLeftMessage, noMoreTriesMessage, difficultyLevelMessage, selectDifficultyMessage, difficultySelectedMessage)
import Types                                  (Model(..), Action(..), GameState(..), GameState(InGame, Sleep), DifficultyLevel(..), Language(..))

-- get number of attempts based on difficulty level
getAttemptsForDifficulty :: DifficultyLevel -> Int
getAttemptsForDifficulty Easy = 6
getAttemptsForDifficulty Medium = 5
getAttemptsForDifficulty Hard = 4

-- parse Telegram update to Action
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ =
  parseUpdate $
      Start <$ command (pack "start")                     -- /start command
      <|> Help <$ command (pack "help")                   -- /help command
      <|> StartGame <$ command (pack "game")              -- /game command
      <|> StopGame <$ command (pack "stop")               -- /stop command
      <|> DifficultyLevel <$ command (pack "difficulty_level") -- /difficulty_level command
      <|> (parsePlain <$> plainText)                       -- parse plain text messages (buttons)
  where 
    -- handle reply keyboard buttons text
    parsePlain :: Text -> Action
    parsePlain text
      | text == pack "Easy" = SelectDifficulty Easy
      | text == pack "Medium" = SelectDifficulty Medium
      | text == pack "Hard" = SelectDifficulty Hard
      | text == pack "🇷🇺 Русский" = SetLanguage Ru
      | text == pack "🇺🇸 English" = SetLanguage En
      | otherwise = TextMessage text

-- handle Action, update Model and produce Effects
handleAction :: Action -> Model -> Eff Action Model
handleAction action model = 
  case action of
    Start -> handleStart model                       -- start command handler
    Help -> handleHelp model                         -- help command handler
    StartGame -> handleStartGame model               -- start game handler
    StopGame -> handleStopGame model                 -- stop game handler
    StartGameWithWord word ->                        -- start game with selected word
        let attempts = getAttemptsForDifficulty (currentDifficulty model)
            newModel = model { gameState = InGame word attempts []}
        in newModel <# replyText (startGameMessage (language model))
    TextMessage word -> handleGuess model word      -- handle guess word
    DifficultyLevel -> handleDifficultyLevel model  -- show difficulty selection keyboard
    SelectDifficulty diff -> handleSelectDifficulty diff model -- handle difficulty selection
    SetLanguage lang -> handleSelectLanguage lang model        -- handle language change

-- send start message with reply keyboard
handleStart :: Model -> Eff Action Model
handleStart model = model <# do
  reply $
    (toReplyMessage (startMessage (language model)))
      { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup (wordleKeyboard (language model))) }

-- send help message
handleHelp :: Model -> Eff Action Model
handleHelp model =
  model <# do
    replyText (helpMessage (language model))

-- start new game by requesting random word
handleStartGame :: Model -> Eff Action Model
handleStartGame model = 
  model <# liftIO (getRandomWordForDifficulty (language model) (currentDifficulty model) >>= return . StartGameWithWord)

-- show difficulty selection keyboard
handleDifficultyLevel :: Model -> Eff Action Model
handleDifficultyLevel model =
  model <# do
    reply $
      (toReplyMessage (selectDifficultyMessage (language model)))
        { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup difficultyKeyboard) }

-- set new difficulty level and reset game state
handleSelectDifficulty :: DifficultyLevel -> Model -> Eff Action Model
handleSelectDifficulty diff model =
  let newModel = model { currentDifficulty = diff, gameState = Sleep }
  in newModel <# do
      reply $
        (toReplyMessage (difficultySelectedMessage (language model) diff))
          { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup (wordleKeyboard (language model))) }

-- toggle language between English and Russian, reset game state
handleSelectLanguage :: Language -> Model -> Eff Action Model
handleSelectLanguage _ model =
  let newLang = case language model of
        En -> Ru
        Ru -> En
      newModel = model { language = newLang, gameState = Sleep }
      msg = case newLang of
        En -> pack "Language changed to English"
        Ru -> pack "Язык изменён на русский"
  in newModel <# do
       reply $
         (toReplyMessage msg)
           { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup (wordleKeyboard newLang)) }

-- stop the game or send fail message if no game running
handleStopGame :: Model -> Eff Action Model
handleStopGame model =
  case gameState model of
    InGame {} ->
      model {gameState = Sleep} <# do
        reply $
          (toReplyMessage (stopGameMessage (language model)))
            { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup (wordleKeyboard (language model))) }
    Sleep ->
      model <# do
        replyText (stopGameFailMessage (language model))

-- handle a guess word during game
handleGuess :: Model -> Text -> Eff Action Model
handleGuess model guessedRaw =
  let guessed = T.toLower guessedRaw
  in case gameState model of
    Sleep -> pure model                                         -- if no game, ignore guess
    InGame target cnt guesslist
      | not (isInList (language model) guessed) ->             -- guess not in word list
        model <# do
          replyText (notInListMessage (language model))
      | guessed == target ->                                   -- correct guess
        let
          newGuesses = guesslist ++ [guessed]
          allFeedback = T.intercalate (pack "\n") $
            zipWith (\guess fb -> fb <> pack " - " <> guess)
                    newGuesses
                    (map (`getColorFeedback` target) newGuesses)
        in model { gameState = Sleep } <# do
          reply $
            (toReplyMessage (allFeedback <> gameWonMessage (language model)))
              { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup (wordleKeyboard (language model))) }
      | otherwise ->                                           -- wrong guess
        let
          newGuesses = guesslist ++ [guessed]
          allFeedback = T.intercalate (pack "\n") $
            zipWith (\guess fb -> fb <> pack " - " <> guess)
                    newGuesses
                    (map (`getColorFeedback` target) newGuesses)
        in
          if cnt <= 1                                            -- no more tries left
            then model { gameState = Sleep } <# do
              reply $
                (toReplyMessage (allFeedback <> noMoreTriesMessage (language model) <> target))
                  { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup (wordleKeyboard (language model))) }
            else model { gameState = InGame target (cnt - 1) newGuesses } <# do
              replyText (allFeedback <> triesLeftMessage (language model) <> pack (show (cnt - 1)))

-- generate color feedback for guess compared to correct word
getColorFeedback :: Text -> Text -> Text
getColorFeedback guessed correct =
  let
    greenSticker = pack "🟩"
    yellowSticker = pack "🟨"
    graySticker = pack "⬜"

    -- mark greens and collect letters not green
    greensAndRemainder :: [Char] -> [Char] -> ([Char], [Char])
    greensAndRemainder [] [] = ([], [])
    greensAndRemainder (g:gs) (c:cs)
      | g == c    = let (gsRes, rem) = greensAndRemainder gs cs
                    in ('G':gsRes, rem)
      | otherwise = let (gsRes, rem) = greensAndRemainder gs cs
                    in ('B':gsRes, c:rem)

    -- remove first occurrence of a letter from list
    removeFirst :: Eq a => a -> [a] -> [a]
    removeFirst _ [] = []
    removeFirst x (y:ys)
      | x == y    = ys
      | otherwise = y : removeFirst x ys

    -- assign colors: green, yellow or gray based on guessed letters and remainder
    yellows :: [Char] -> [Char] -> [Char] -> [Text]
    yellows [] [] _ = []
    yellows (g:gs) (m:ms) rem
      | m == 'G'     = greenSticker : yellows gs ms rem
      | g `elem` rem = yellowSticker : yellows gs ms (removeFirst g rem)
      | otherwise    = graySticker : yellows gs ms rem

    (greenMarks, remainder) = greensAndRemainder (unpack guessed) (unpack correct)
    feedback = yellows (unpack guessed) greenMarks remainder
  in T.concat feedback

-- keyboard for difficulty selection
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

-- main wordle keyboard depending on language
wordleKeyboard :: Language -> Telegram.ReplyKeyboardMarkup
wordleKeyboard lang =
  Telegram.ReplyKeyboardMarkup
    { Telegram.replyKeyboardMarkupKeyboard =
        [ [ Telegram.KeyboardButton (pack "/game") Nothing Nothing Nothing Nothing Nothing Nothing
          , Telegram.KeyboardButton (pack "/help") Nothing Nothing Nothing Nothing Nothing Nothing
          ]
        , [ Telegram.KeyboardButton (pack langText) Nothing Nothing Nothing Nothing Nothing Nothing
          , Telegram.KeyboardButton (pack "/difficulty_level") Nothing Nothing Nothing Nothing Nothing Nothing
          ]
        ]
    , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
    , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just True
    , Telegram.replyKeyboardMarkupSelective = Nothing
    , Telegram.replyKeyboardMarkupInputFieldSelector = Nothing
    , Telegram.replyKeyboardMarkupIsPersistent = Nothing
    }
  where
    langText = case lang of
      En -> "🇺🇸 English"
      Ru -> "🇷🇺 Русский"
