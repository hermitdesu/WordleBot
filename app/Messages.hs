module Messages (startMessage, helpMessage, stopGameMessage, stopGameFailMessage, startGameMessage, gameWonMessage, triesLeftMessage, noMoreTriesMessage, notInListMessage) where

import Data.Text (Text, pack)

startMessage :: Text
startMessage = pack "Hello, i'm WordleBot! To start the game type /game."

helpMessage :: Text
helpMessage = pack"Wordle is game in which you need to guess the word by receiving feedback on letters. \n\
                  \🟩 - letter is present in right position \n\
                  \🟨 - letter is present \n\
                  \⬛ - no such letter \n\
                  \Available commands: \n\
                  \/game - to start the game \n\
                  \/stop - to stop the game \n\
                  \/help - inforamtion about commands \n\
                  \/start - to restart bot."

startGameMessage :: Text
startGameMessage = pack "Game started!"

stopGameMessage :: Text
stopGameMessage = pack "Game was aborted."

stopGameFailMessage :: Text
stopGameFailMessage = pack "You are not in game."

gameWonMessage :: Text
gameWonMessage = pack "\nYou have won, congrats!"

notInListMessage :: Text
notInListMessage = pack "No such word in our list of words."

triesLeftMessage :: Text
triesLeftMessage = pack "\nTries left: "

noMoreTriesMessage :: Text
noMoreTriesMessage = pack "\nNo more tries left, you lost( \nCorrect word was: "