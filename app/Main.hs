module Main (main) where

import Configuration.Dotenv                   (defaultConfig, loadFile)
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug              (traceBotDefault)
import System.Environment                     (getEnv)
import Data.Text as T                         (pack)

import Types                                  (Model(..), Action(..), GameState(..), GameState(Sleep))
import Handlers (handleUpdate, handleAction)


bot :: BotApp Model Action
bot =
  BotApp
    { botInitialModel = Model Sleep,
      botAction = flip handleUpdate,
      botHandler = handleAction,
      botJobs = []
    }

run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault (conversationBot Telegram.updateChatId bot)) env


main :: IO ()
main = do
  _ <- loadFile defaultConfig
  token <- getEnv "TELEGRAM_TOKEN"
  run (Telegram.Token (pack token))