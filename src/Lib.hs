module Lib
  ( WordleGame(..)
  , GameStatus(..)
  , GuessResult(..)
  , newGame
  , makeGuess
  , isValidWord
  , getWordList
  , getRandomWord
  , formatGameBoard
  , validateGuess
  , calculateFeedback
  ) where

import Data.Char (isLetter)
import System.Random (randomRIO)
import Data.Text (Text, pack, toLower, unpack)
import qualified Data.Text as T

-- Список 5-буквенных слов для игры
wordList :: [Text]
wordList = map pack
  [ "apple", "beach", "chair", "dance", "eagle", "flame", "grape", "heart"
  , "image", "juice", "knife", "lemon", "music", "night", "ocean", "peace"
  ]

data GameStatus
  = InProgress
  | Won
  | Lost
  deriving (Show, Eq)

data GuessResult
  = Correct
  | Incorrect [Char]
  | InvalidGuess String
  deriving (Show)

data WordleGame = WordleGame
  { targetWord :: Text
  , maxAttempts :: Int
  , currentAttempt :: Int
  , guesses :: [Text]
  , status :: GameStatus
  } deriving (Show)

newGame :: Text -> WordleGame
newGame word = WordleGame
  { targetWord = toLower word
  , maxAttempts = 6
  , currentAttempt = 0
  , guesses = []
  , status = InProgress
  }

getRandomWord :: IO Text
getRandomWord = do
  index <- randomRIO (0, length wordList - 1)
  return $ wordList !! index

getWordList :: [Text]
getWordList = wordList

isValidWord :: Text -> Bool
isValidWord word = 
  let wordLower = toLower word
  in T.length wordLower == 5 
     && all isLetter (unpack wordLower)
     && wordLower `elem` wordList

validateGuess :: Text -> Either String Text
validateGuess word
  | T.length word /= 5 = Left " Слово должно содержать ровно 5 букв"
  | not (all isLetter (unpack word)) = Left " Используйте только буквы"
  | not (isValidWord word) = Left " Слово не найдено в словаре"
  | otherwise = Right (toLower word)

makeGuess :: WordleGame -> Text -> (WordleGame, GuessResult)
makeGuess game guess
  | status game /= InProgress = (game, InvalidGuess " Игра уже завершена")
  | otherwise = 
      case validateGuess guess of
        Left errorMsg -> (game, InvalidGuess errorMsg)
        Right validGuess ->
          let result = checkGuess (targetWord game) validGuess
              newAttempt = currentAttempt game + 1
              newGuesses = guesses game ++ [validGuess]
              newStatus = if validGuess == targetWord game 
                         then Won 
                         else if newAttempt >= maxAttempts game 
                              then Lost 
                              else InProgress
              updatedGame = game 
                { currentAttempt = newAttempt
                , guesses = newGuesses
                , status = newStatus
                }
          in (updatedGame, result)

checkGuess :: Text -> Text -> GuessResult
checkGuess target guess
  | target == guess = Correct
  | otherwise = Incorrect (calculateFeedback target guess)

calculateFeedback :: Text -> Text -> [Char]
calculateFeedback target guess = 
  let targetChars = unpack target
      guessChars = unpack guess
      exactMatches = [(i, c) | (i, c) <- zip [0..] guessChars, 
                               i < length targetChars && c == targetChars !! i]
      exactPositions = map fst exactMatches
      exactLetters = map snd exactMatches
      remainingTarget = [c | (i, c) <- zip [0..] targetChars, i `notElem` exactPositions]
      partialMatches = [c | c <- guessChars, c `elem` remainingTarget, c `notElem` exactLetters]
      result = zipWith (\i c -> 
        if i `elem` exactPositions then '🟩'
        else if c `elem` partialMatches then '🟨'
        else '⬛') [0..] guessChars
  in result

formatGameBoard :: WordleGame -> Text
formatGameBoard game = 
  let attempts = guesses game
      maxTry = 6
      emptyRows = replicate (maxTry - length attempts) "⬜⬜⬜⬜⬜"
      filledRows = map formatGuess attempts
      allRows = filledRows ++ emptyRows
  in pack $ unlines allRows
  where
    formatGuess guess = 
      let feedback = calculateFeedback (targetWord game) guess
      in unwords [unpack guess, "→", feedback]
