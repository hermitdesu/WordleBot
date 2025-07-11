module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Configuration.Dotenv (defaultConfig, loadFile)
import WordList (getRandomWord)
import Control.Applicative
import Data.Char
import Data.Text as T
import System.Environment (getEnv)
import Telegram.Bot.API
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug
import Telegram.Bot.Simple.UpdateParser

data GameState
  = NotPlaying
  | WaitingForAnswer Text Int
  deriving (Show)

data AnswerResult
  = InvalidInput Text
  | WrongAnswer Text
  | CorrectAnswer Text

data Model = Model
  { gameState :: GameState
  }
  deriving (Show)

data Action
  = Start
  | Help
  | StartGame
  | StopGame
  | StartGameWithWord Text
  | TextMessage Text
  deriving (Show)

bot :: BotApp Model Action
bot =
  BotApp
    { botInitialModel = Model {gameState = NotPlaying},
      botAction = flip handleUpdate,
      botHandler = handleAction,
      botJobs = []
    }
  where
    handleUpdate :: Model -> Telegram.Update -> Maybe Action
    handleUpdate _ =
      parseUpdate $
        Start <$ command (pack "start")
          <|> Help <$ command (pack "help")
          <|> StartGame <$ command (pack "game")
          <|> StopGame <$ command (pack "stop")
          <|> TextMessage <$> text

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model =
      case action of
        Start ->
          model <# do
            reply $
              (toReplyMessage (pack "Hello, i'm WordleBot! To start the game type /game."))
                { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup wordleKeyboard)
                }
        Help ->
          model <# do
            replyText
              ( pack
                  "Wordle is game in which you need to guess the word by receiving feedback on letters. \n\
                  \🟩 - letter is present in right position \n\
                  \🟨 - letter is present \n\
                  \⬛ - no such letter \n\
                  \Available commands: \n\
                  \/game - to start the game \n\
                  \/stop - to stop the game \n\
                  \/help - inforamtion about commands \n\
                  \/start - to restart bot."
              )
        StartGame ->
          model <# liftIO (StartGameWithWord <$> getRandomWord)
        StartGameWithWord word ->
          model {gameState = WaitingForAnswer word 5} <# do
            replyText (pack "Guess the word!")
        StopGame ->
          case gameState model of
            WaitingForAnswer _ _ ->
              model {gameState = NotPlaying} <# do
                replyText (pack "Game finished.")
            NotPlaying ->
              model <# do
                replyText (pack "You are not in game.")
        TextMessage msg ->
          case gameState model of
            NotPlaying ->
              model <# do
                replyText (pack "I do not understand")
            WaitingForAnswer word cnt ->
              case validateAnswer (T.toLower word) (T.toLower msg) of
                InvalidInput feedback ->
                  model <# do
                    replyText feedback
                WrongAnswer feedback ->
                  let new_cnt = cnt - 1
                   in if new_cnt <= 0
                        then
                          model {gameState = NotPlaying} <# do
                            reply $
                              (toReplyMessage (feedback <> pack "\nYou lost, no more tries."))
                                {replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup wordleKeyboard)}
                        else
                          model {gameState = WaitingForAnswer word new_cnt} <# do
                            replyText (feedback <> pack "\nTries left " <> pack (show new_cnt) <> pack "/5")
                CorrectAnswer feedback ->
                  model <# do
                    reply $
                      (toReplyMessage (feedback <> pack "\nCongrats you won"))
                        {replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup wordleKeyboard)}

    wordleKeyboard :: ReplyKeyboardMarkup
    wordleKeyboard =
      ReplyKeyboardMarkup
        { replyKeyboardMarkupKeyboard =
            [ [ KeyboardButton (pack "/game") Nothing Nothing Nothing Nothing Nothing Nothing,
                KeyboardButton (pack "/help") Nothing Nothing Nothing Nothing Nothing Nothing
              ]
            ],
          replyKeyboardMarkupResizeKeyboard = Just True,
          replyKeyboardMarkupOneTimeKeyboard = Just True,
          replyKeyboardMarkupSelective = Nothing,
          replyKeyboardMarkupInputFieldSelector = Nothing,
          replyKeyboardMarkupIsPersistent = Nothing
        }

validateAnswer :: Text -> Text -> AnswerResult
validateAnswer correct guessed
  | not (T.all isLetter guessed) = InvalidInput (pack "Only letters are allowed")
  | T.length guessed /= 5 = InvalidInput (pack "Word should be 5 letters")
  | guessed == correct = CorrectAnswer (answerFeedBack correct guessed)
  | otherwise = WrongAnswer (answerFeedBack correct guessed)

answerFeedBack :: Text -> Text -> Text
answerFeedBack correct guessed = emojiLine
  where
    charFeedBack :: Char -> Char -> Char
    charFeedBack c g
      | c == g = '🟩'
      | g `T.elem` correct = '🟨'
      | otherwise = '⬛'

    emojiLine = T.zipWith charFeedBack correct guessed

run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault (conversationBot Telegram.updateChatId bot)) env

main :: IO ()
main = do
  _ <- loadFile defaultConfig
  token <- getEnv "TELEGRAM_TOKEN"
  run (Telegram.Token (pack token))
