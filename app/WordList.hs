module WordList (getRandomWord, isInList, getRandomWordForDifficulty, isInListForDifficulty) where

import System.Random (randomRIO)
import Data.Text (Text, pack)
import Types (DifficultyLevel(..))

-- Легкие слова (простые, часто используемые)
easyWordList :: [Text]
easyWordList =
  map pack
  [ "apple", "beach", "chair", "dance", "eagle", "flame", "grape", "heart"
  , "image", "juice", "knife", "lemon", "music", "night", "ocean", "peace"
  , "queen", "river", "smile", "table", "unity", "voice", "water", "youth"
  , "bread", "clock", "dream", "earth", "field", "green", "house", "light"
  ]

-- Средние слова (более сложные, но все еще понятные)
mediumWordList :: [Text]
mediumWordList =
  map pack
  [ "abuse", "adult", "agent", "anger", "apple", "award", "beach", "birth"
  , "block", "blood", "board", "brain", "bread", "break", "breed", "brief"
  , "bring", "broad", "broke", "brown", "build", "built", "buyer", "cable"
  , "calif", "carry", "catch", "cause", "chain", "chair", "chart", "chase"
  , "cheap", "check", "chest", "chief", "child", "china", "chose", "civil"
  , "claim", "class", "clean", "clear", "click", "climb", "clock", "close"
  ]

-- Сложные слова (редкие, технические)
hardWordList :: [Text]
hardWordList =
  map pack
  [ "abyss", "adobe", "agile", "alibi", "amaze", "anvil", "attic", "azure"
  , "blitz", "bloom", "bluff", "bogus", "bonus", "booth", "bound", "brave"
  , "brawl", "brawn", "braze", "bread", "break", "breed", "brick", "bride"
  , "brief", "bring", "brisk", "broad", "broke", "brown", "build", "built"
  , "buyer", "cable", "calif", "carry", "catch", "cause", "child", "china"
  , "chose", "civil", "claim", "class", "clean", "clear", "click", "climb"
  , "clock", "close", "coach", "coast", "could", "court", "delay", "delta"
  , "delve", "demon", "denim", "dense", "depot", "depth", "derby", "diary"
  ]

-- Получение списка слов в зависимости от уровня сложности
getWordListForDifficulty :: DifficultyLevel -> [Text]
getWordListForDifficulty Easy = easyWordList
getWordListForDifficulty Medium = mediumWordList
getWordListForDifficulty Hard = hardWordList

-- Получение случайного слова для определенного уровня сложности
getRandomWordForDifficulty :: DifficultyLevel -> IO Text
getRandomWordForDifficulty difficulty = do
  let wordList = getWordListForDifficulty difficulty
  idx <- randomRIO (0, length wordList - 1)
  return $ wordList !! idx

-- Получение случайного слова из основного списка (для обратной совместимости)
getRandomWord :: IO Text
getRandomWord = getRandomWordForDifficulty Medium

-- Проверка, есть ли слово в списке для определенного уровня сложности
isInListForDifficulty :: DifficultyLevel -> Text -> Bool
isInListForDifficulty difficulty word = word `elem` getWordListForDifficulty difficulty

-- Проверка, есть ли слово в основном списке (для обратной совместимости)
isInList :: Text -> Bool
isInList = isInListForDifficulty Medium