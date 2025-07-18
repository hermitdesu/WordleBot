module Messages (startMessage, helpMessage, stopGameMessage, stopGameFailMessage, startGameMessage, gameWonMessage, triesLeftMessage, noMoreTriesMessage, notInListMessage, difficultyLevelMessage, selectDifficultyMessage, difficultySelectedMessage) where

import Data.Text (Text, pack)
import Types (DifficultyLevel(..))

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
                  \/start - to restart bot\n\
                  \/difficulty_level - set difficulty level."

startGameMessage :: Text
startGameMessage = pack "Game started!"

stopGameMessage :: Text
stopGameMessage = pack "Game was aborted."

stopGameFailMessage :: Text
stopGameFailMessage = pack "You are not in game."

gameWonMessage :: Text
gameWonMessage = pack "\nYou have won, congrats!"

notInListMessage :: Text
notInListMessage = pack "I don't understand.."

triesLeftMessage :: Text
triesLeftMessage = pack "\nTries left: "

noMoreTriesMessage :: Text
noMoreTriesMessage = pack "\nNo more tries left, you lost( \nCorrect word was: "

difficultyLevelMessage :: Text
difficultyLevelMessage = pack "Difficulty level settings:\n\
                  \Easy - 6 attempts\n\
                  \Medium - 5 attempts\n\
                  \Hard - 4 attempts"

selectDifficultyMessage :: Text
selectDifficultyMessage = pack "Select difficulty level:"

difficultySelectedMessage :: DifficultyLevel -> Text
difficultySelectedMessage Easy = pack "Easy difficulty selected! You have 6 attempts."
difficultySelectedMessage Medium = pack "Medium difficulty selected! You have 5 attempts."
difficultySelectedMessage Hard = pack "Hard difficulty selected! You have 4 attempts."