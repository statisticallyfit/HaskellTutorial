module Hangman where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
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