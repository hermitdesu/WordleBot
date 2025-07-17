module WordList (wordList, getRandomWord) where

import System.Random (randomRIO)
import Data.Text (Text, pack)

-- Статический список слов из 5 букв
wordList :: [Text]
wordList =
  map pack
  [ "apple", "beach", "chair", "dance", "eagle", "flame", "grape", "heart"
  , "image", "juice", "knife", "lemon", "music", "night", "ocean", "peace"
  ]
-- Получение случайного слова из списка
getRandomWord :: IO Text
getRandomWord = do
  idx <- randomRIO (0, length wordList - 1)
  return $ wordList !! idx