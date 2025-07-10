module Main (main) where

import Telegram.Bot.Simple

data Model = Model

data Action
    = DoNothing

bot :: BotApp Model Action
bot = BotApp {}

main :: IO ()
main = someFunc
