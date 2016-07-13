module Hangman where

import Control.Monad -- (forever)
import Data.Char -- (toLower)
import Data.Maybe -- (isJust)
import Data.List
import System.Exit (exitSuccess)
import System.Random (randomRIO)

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic



type WordList = [String]

filepath :: String
filepath = "/datascience/projects/statisticallyfit/github/learningprogramming/" ++
           "Haskell/HaskellTutorial/src/Books/ChrisAllen_HaskellFirstPrinciples/" ++
           "chapter13_BuildingProjects/data/dict.txt"


minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

triesAllowed :: Int
triesAllowed = 6

hangman :: [String]
hangman = ["head", "body", "left foot", "right foot", "arms", "mouth", "eyes"]


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
data Puzzle = Puzzle String [Maybe Char] [Char] deriving Eq
--                    [1]      [2]         [3]
-- [1] = word we are trying to guess
-- [2] = characters filled in so far
-- [3] = all letters guessed so far


instance Show Puzzle where
    show (Puzzle _ discovered guesses) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guesses


freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word noneDiscovered []
    where noneDiscovered = map (\_ -> Nothing) word
        -- or could have used: [Nothing | _ <- word]


-- note checks whether guessed char is element of the word.
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) guessLetter = elem guessLetter word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guesses) guessLetter = elem guessLetter guesses


renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '_'

-- note inserts correctly guessLetter into the string
-- uses zipper function which:
-- 1. if the guessLetter equals the word char, then return the guessLetter
 -- in a Just.
-- 2. Otherwise, just return the discovered char because it is either a Nothing
-- or Just (previously discovered char)
-- note gc = guessedChar = charToAdd
--      wc = wordChar (word letters one at a time)
--      dc = discoveredChar (discovered maybes one at a time)
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word discovered guesses) guessLetter =
    Puzzle word newDiscovered (guessLetter : guesses)
    where zipper gc wc dc = if wc == gc then Just wc else dc
          newDiscovered = zipWith (zipper guessLetter) word discovered
-- note: if gc == wc then input a Just wc at that spot in ddiscovered list.
-- else just return the previously discovered Just or Nothing at that spot.


numWrongGuesses :: Puzzle -> Int
numWrongGuesses p@(Puzzle word _ guesses)
    = fromIntegral $ length guesses - guessesInWord
    where guessesInWord
            = fromIntegral $ length $ filter ((flip elem) word) guesses



-- note tells player what he/she guessed.
-- handles cases: 1) guessLetter was guessed before
--                2) guessLetter is in word and needs to be filled in
--                3) guessLetter was not previously guessed and wasn't in word.
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guessLetter = do
    putStrLn $ "Your guess was: " ++ [guessLetter]
    case (charInWord puzzle guessLetter, alreadyGuessed puzzle guessLetter) of
        (_, True) -> do putStrLn "ALREADY GUESSED, choose another!"
                        return puzzle
        (True, _) -> do putStrLn "MATCH! Filling in ..."
                        return (fillInCharacter puzzle guessLetter)
        (False,_) -> do putStrLn "TRY AGAIN"
                        return (fillInCharacter puzzle guessLetter)



-- note game stops only after seven guesses (either incorrect or correct)
-- note help todo: I put the current wrong/tries left/lumbs hung information
-- here not in handleGuess function ebcause this func comes before handleGuesss
-- and I want this info to be on top of printed puzzle (from handleGuess)
gameOver :: Puzzle -> IO()
gameOver p@(Puzzle word discovered guesses) = do
    let wrongsMade = numWrongGuesses p
    let triesLeft = triesAllowed - wrongsMade + 1
    let limb = if wrongsMade == 0
               then "_"
               else hangman !! (wrongsMade - 1)
    putStrLn ("Current wrong: " ++ show wrongsMade)
    putStrLn ("Tries left: " ++ show triesLeft)
    putStrLn ("Limb hung: " ++ limb)
    if (numWrongGuesses p) > triesAllowed then -- gets 7 tries. If 7th is wrong, game over.
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ word
           exitSuccess
    else return ()

-- note game is won when there are no more Nothings in the discovered pile.
gameWin :: Puzzle -> IO()
gameWin (Puzzle word discovered _) =
    if all isJust discovered then
        do putStrLn "You win!"
           putStrLn ("The word was: " ++ word)
           exitSuccess
    else return ()


runGame :: Puzzle -> IO()
runGame puzzle = forever $ do
    putStrLn ""
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guessLetter <- getLine
    case guessLetter of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "BAD INPUT: Your guess must be a single character."



------------------------------------------------------------------------------------
main :: IO()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
------------------------------------------------------------------------------------
