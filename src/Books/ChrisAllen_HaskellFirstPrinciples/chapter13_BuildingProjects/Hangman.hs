module Hangman where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List
import System.Exit (exitSuccess)
import System.Random (randomRIO)




type WordList = [String]

filepath :: String
filepath = "/datascience/projects/statisticallyfit/github/learningprogramming/" ++
           "Haskell/HaskellTutorial/src/Books/ChrisAllen_HaskellFirstPrinciples/" ++
           "chapter13_BuildingProjects/data/dict.txt"


minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9




-- note lines splits string at '\n'
-- words splits words at '\n' AND ' '
allWords :: IO WordList
allWords = do
    dict <- readFile filepath
    return (lines dict)

-- note filtering words by min and max length above
gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w =
            let l = length (w :: String)
            in l > minWordLength && l < maxWordLength


randomWord :: WordList -> IO String
randomWord wordList = do
    randomIndex <- randomRIO(0, (length wordList - 1))
    return (wordList !! randomIndex)


randomWord' :: IO String
randomWord' = gameWords >>= randomWord




------------------------------------------------------------------------------------
data Puzzle = Puzzle String [Maybe Char] [Char]
--                    [1]      [2]         [3]
-- [1] = word we are trying to guess
-- [2] = characters filled in so far
-- [3] = all letters guessed so far

{-

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed
-}

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word noneDiscovered []
    where noneDiscovered = map (\_ -> Nothing) word
        -- or could have used: [Nothing | _ <- word]



-- note checks whether guessed char is element of the word.
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) g = elem g word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) g = elem g guessed


renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '_'

-- note inserts correctly guessed char into the string
-- uses zipper function which: if the guessChar equals the word char, then
-- return the guessChar in a Just. Otherwise, just return the discovered char
-- because it is either a Nothing or  Just (previously discovered char)
-- note gc = guessedChar = charToAdd
--      wc = wordChar (word letters one at a time)
--      dc = discoveredChar (discovered maybes one at a time)
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word discovered guesses) charToAdd =
    Puzzle word newDiscovered (charToAdd : guesses)
    where zipper gc wc dc = if wc == gc then Just wc else dc
          newDiscovered = zipWith (zipper charToAdd) word discovered
