module Messages
  ( startMessage
  , helpMessage
  , stopGameMessage
  , stopGameFailMessage
  , startGameMessage
  , gameWonMessage
  , triesLeftMessage
  , noMoreTriesMessage
  , notInListMessage
  , difficultyLevelMessage
  , selectDifficultyMessage
  , difficultySelectedMessage
  ) where

import Data.Text (Text, pack)
import Types (DifficultyLevel(..), Language(..))

-- functions to get text for messages on russian or english

startMessage :: Language -> Text
startMessage lang = case lang of
  En -> pack "Hello, I'm WordleBot! To start the game type /game."
  Ru -> pack "Привет! Я WordleBot! Чтобы начать игру, напиши /game."

helpMessage :: Language -> Text
helpMessage lang = case lang of
  En -> pack "Wordle is a game where you guess the word by receiving feedback on letters.\n\
             \🟩 - letter in correct position\n\
             \🟨 - letter is present but misplaced\n\
             \⬛ - letter not in word\n\
             \Available commands:\n\
             \/game - start the game\n\
             \/stop - stop the game\n\
             \/help - command information\n\
             \/start - restart the bot\n\
             \/difficulty_level - set difficulty level."
  Ru -> pack "Wordle — это игра, где ты угадываешь слово, получая подсказки:\n\
             \🟩 — буква на правильной позиции\n\
             \🟨 — буква есть, но не на месте\n\
             \⬛ — такой буквы нет\n\
             \Доступные команды:\n\
             \/game — начать игру\n\
             \/stop — остановить игру\n\
             \/help — информация о командах\n\
             \/start — перезапустить бота\n\
             \/difficulty_level — выбрать уровень сложности."

startGameMessage :: Language -> Text
startGameMessage lang = case lang of
  En -> pack "Game started!"
  Ru -> pack "Игра началась!"

stopGameMessage :: Language -> Text
stopGameMessage lang = case lang of
  En -> pack "Game was aborted."
  Ru -> pack "Игра была остановлена."

stopGameFailMessage :: Language -> Text
stopGameFailMessage lang = case lang of
  En -> pack "You are not in game."
  Ru -> pack "Сейчас вы не в игре."

gameWonMessage :: Language -> Text
gameWonMessage lang = case lang of
  En -> pack "\nYou have won, congrats!"
  Ru -> pack "\nВы победили, поздравляем!"

notInListMessage :: Language -> Text
notInListMessage lang = case lang of
  En -> pack "There is no such word in our wordlist."
  Ru -> pack "Такого слова нет в нашем словаре."

triesLeftMessage :: Language -> Text
triesLeftMessage lang = case lang of
  En -> pack "\nTries left: "
  Ru -> pack "\nОсталось попыток: "

noMoreTriesMessage :: Language -> Text
noMoreTriesMessage lang = case lang of
  En -> pack "\nNo more tries left, you lost( \nCorrect word was: "
  Ru -> pack "\nПопытки закончились, вы проиграли( \nПравильное слово: "

difficultyLevelMessage :: Language -> Text
difficultyLevelMessage lang = case lang of
  En -> pack "Difficulty level settings:\n\
             \Easy - 6 attempts\n\
             \Medium - 5 attempts\n\
             \Hard - 4 attempts"
  Ru -> pack "Настройки уровня сложности:\n\
             \Легкий — 6 попыток\n\
             \Средний — 5 попыток\n\
             \Сложный — 4 попытки"

selectDifficultyMessage :: Language -> Text
selectDifficultyMessage lang = case lang of
  En -> pack "Select difficulty level:"
  Ru -> pack "Выберите уровень сложности:"

difficultySelectedMessage :: Language -> DifficultyLevel -> Text
difficultySelectedMessage lang level = case lang of
  En -> case level of
    Easy   -> pack "Easy difficulty selected! You have 6 attempts."
    Medium -> pack "Medium difficulty selected! You have 5 attempts."
    Hard   -> pack "Hard difficulty selected! You have 4 attempts."
  Ru -> case level of
    Easy   -> pack "Выбран лёгкий уровень! У вас 6 попыток."
    Medium -> pack "Выбран средний уровень! У вас 5 попыток."
    Hard   -> pack "Выбран сложный уровень! У вас 4 попытки."
