module Main (main) where

import Data.Text (Text, pack)

import Telegram.Bot.Simple
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple.Debug (traceBotDefault)


data Model = Model
    deriving (Show)

data Action 
    = DoNothing
    | Echo Text
    deriving (Show)


bot :: BotApp Model Action
bot = BotApp 
    {   botInitialModel = Model
    ,   botAction = handleUpdate
    ,   botHandler = handleAction
    ,   botJobs = []
    }


handleUpdate :: Telegram.Update -> Model -> Maybe Action
handleUpdate _ _ = Just (Echo (pack "Hello"))


handleAction :: Action -> Model -> Eff Action Model
handleAction action model = 
    case action of
        DoNothing -> pure model
        Echo msg -> model <# do
            replyText msg
            pure DoNothing


run :: Telegram.Token -> IO ()
run token = do
    env <- Telegram.defaultTelegramClientEnv token
    startBot_ (traceBotDefault bot) env


main :: IO ()
main = getEnvToken "TELEGRAM_TOKEN" >>= run