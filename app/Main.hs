module Main (main) where

import Control.Applicative
import Data.Char
import Data.Text as T
import Telegram.Bot.API ()
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug
import Telegram.Bot.Simple.UpdateParser

data GameState
  = NotPlaying
  | WaitingForAnswer Text
  deriving (Show)

data Model = Model
  { gameState :: GameState
  }
  deriving (Show)

data Action
  = Start
  | Help
  | StartGame
  | StopGame
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
            replyText (pack "hello this is bot")
        Help ->
          model <# do
            replyText (pack "this is help")
        StartGame ->
          model {gameState = WaitingForAnswer (pack "apple")} <# do
            replyText (pack "new game")
        StopGame ->
          case gameState model of
            WaitingForAnswer _ ->
              model {gameState = NotPlaying} <# do
                replyText (pack "game finished")
            NotPlaying ->
              model <# do
                replyText (pack "you are not in game")
        TextMessage msg ->
          case gameState model of
            NotPlaying ->
              model <# do
                replyText (pack "I do not understand")
            WaitingForAnswer word ->
              if T.toLower word /= T.toLower msg
                then
                  model <# do
                    replyText (validateAnswer word msg)
                else
                  model {gameState = NotPlaying} <# do
                    replyText (pack "you got the right answer")

validateAnswer :: Text -> Text -> Text
validateAnswer correct guessed
  | not (T.all isLetter guessed) = pack "only letters are allowed"
  | T.length guessed /= 5 = pack "word should be 5 letters" <> correct
  | otherwise = pack "incorrect"

run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault (conversationBot Telegram.updateChatId bot)) env

main :: IO ()
main = getEnvToken "TELEGRAM_TOKEN" >>= run
