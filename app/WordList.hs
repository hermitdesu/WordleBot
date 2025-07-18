module WordList (getRandomWord, isInList, getRandomWordForDifficulty, isInListForDifficulty) where

import System.Random (randomRIO)
import Data.Text (Text, pack, unpack)
import Types (DifficultyLevel(..), Language(..))


-- Фильтр для слов строго из 5 букв
filterFiveLetters :: [Text] -> [Text]
filterFiveLetters = filter ((==5) . length . unpack)

-- Английские слова по уровням сложности (все по 5 букв)
englishEasyWords :: [Text]
englishEasyWords =
  filterFiveLetters $ map pack
  [ "apple", "beach", "chair", "dance", "eagle", "flame", "grape", "heart"
  , "image", "juice", "knife", "lemon", "music", "night", "ocean", "peace"
  , "queen", "river", "smile", "table", "unity", "voice", "water", "youth"
  , "bread", "clock", "dream", "earth", "field", "green", "house", "light"
  ]

englishMediumWords :: [Text]
englishMediumWords =
  filterFiveLetters $ map pack
  [ "abuse", "adult", "agent", "anger", "award", "birth", "block", "blood"
  , "board", "brain", "break", "breed", "brief", "bring", "broad", "brown"
  , "build", "built", "buyer", "cable", "carry", "catch", "cause", "chain"
  , "chart", "chase", "cheap", "check", "chest", "chief", "child", "china"
  , "chose", "civil", "claim", "class", "clean", "clear", "click", "climb"
  , "close", "coach", "coast", "court", "cover", "craft", "crash", "cream"
  ]

englishHardWords :: [Text]
englishHardWords =
  filterFiveLetters $ map pack
  [ "abyss", "adobe", "agile", "alibi", "amaze", "anvil", "attic", "azure"
  , "blitz", "bloom", "bluff", "bogus", "bonus", "booth", "bound", "brave"
  , "brawl", "brawn", "braze", "brick", "bride", "brisk", "brood", "brook"
  , "broom", "brunt", "buddy", "buggy", "bulky", "bunch", "burly", "butch"
  , "cacao", "cadet", "cameo", "canon", "caper", "carat", "cargo", "carol"
  , "chafe", "chalk", "champ", "chant", "chard", "charm", "chime", "chock"
  ]

-- Русские слова по уровням сложности (все по 5 букв)
russianEasyWords :: [Text]
russianEasyWords =
  filterFiveLetters $ map pack
  [ "берег", "ветер", "гость", "дождь", "жажда", "книга", "мечта"
  , "огонь", "удача", "ягода", "белый", "весна", "груша", "вечер"
  ]

russianMediumWords :: [Text]
russianMediumWords =
  filterFiveLetters $ map pack
  [ "абзац", "балет", "валет", "гараж", "декан", "запах", "икота", "капля"
  , "ларец", "магия", "невод", "облик", "палач", "радар", "салат", "табак"
  , "упрек", "фасон", "хобби", "цапля", "штора", "элита", "юрист", "дрожь"
  , "якорь", "актер", "билет", "вдова"
  ]

russianHardWords :: [Text]
russianHardWords =
  filterFiveLetters $ map pack
  [ "абрек", "бонус", "вьюга", "гюрза", "дрофа", "жерло", "зенит", "кювет"
  , "лямка", "мышца", "озноб", "рэкет", "самум", "тщета", "уксус", "фьорд"
  , "хлыст", "цыган", "чрево", "штырь", "щупак", "этнос", "юниор", "яство"
  , "аорта", "бювет", "вязка"
  ]

getWordList :: Language -> DifficultyLevel -> [Text]
getWordList lang difficulty =
  case (lang, difficulty) of
    (En, Easy) -> englishEasyWords
    (En, Medium) -> englishMediumWords
    (En, Hard) -> englishHardWords
    (Ru, Easy) -> russianEasyWords
    (Ru, Medium) -> russianMediumWords
    (Ru, Hard) -> russianHardWords

getRandomWordForDifficulty :: Language -> DifficultyLevel -> IO Text
getRandomWordForDifficulty lang difficulty = do
  let wordsList = getWordList lang difficulty
  idx <- randomRIO (0, length wordsList - 1)
  return (wordsList !! idx)

isInListForDifficulty :: Language -> DifficultyLevel -> Text -> Bool
isInListForDifficulty lang difficulty word =
  word `elem` getWordList lang difficulty

getRandomWord :: Language -> IO Text
getRandomWord lang = getRandomWordForDifficulty lang Easy

isInList :: Language -> Text -> Bool
isInList lang word = any (\d -> isInListForDifficulty lang d word) [Easy, Medium, Hard]
