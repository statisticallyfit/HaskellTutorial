module Main where

{-
key to load both Main and Morse hs do this in GHCI:
:l src/Books/ChrisAllen_HaskellFirstPrinciples/chapter14_Testing/MorseCode/Main.hs
src/Books/ChrisAllen_HaskellFirstPrinciples/chapter14_Testing/MorseCode/Morse.hs
-}

import Control.Monad (forever, when)
import Data.List (intercalate)
import Data.Traversable (traverse)
import Morse (stringToMorse, morseToChar)
import System.Environment --(getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)



convertToMorse :: IO()
convertToMorse = forever $ do
    weAreDone <- hIsEOF stdin
    when weAreDone exitSuccess
    -- otherwise, proceed
    line <- hGetLine stdin
    convertLine line
    where
    convertLine line = do
        let morse = stringToMorse line
        case morse of
            -- convert from Just [...] to morse text with spaces ".. .. .."
            (Just str) -> putStrLn $ intercalate " " str
            Nothing    -> do
                putStrLn $ "ERROR: " ++ line
                exitFailure


convertFromMorse :: IO()
convertFromMorse = forever $ do
    weAreDone <- hIsEOF stdin
    when weAreDone exitSuccess
    -- otherwise proceed
    line <- hGetLine stdin
    convertLine line
    where
    convertLine line = do
        let decoded :: Maybe String
            decoded = traverse morseToChar (words line)
        case decoded of  -- decoded = Just "this is a sentence"
            (Just sentence) -> putStrLn sentence
            Nothing  -> do
                putStrLn $ "ERROR: " ++ line
                exitFailure



--- help todo - how to get in command line arguments?
main :: IO()
main = do
    modeChosenByUser <- getArgs
    case modeChosenByUser of
        [arg] ->
            case arg of
                "from" -> convertFromMorse
                "to"   -> convertToMorse
                _      -> argError
        _ -> argError
        where argError = do
                putStrLn "Please specify the first argument \
                    \as being 'from' or 'to' morse,\
                    \ such as: morse to"

                --putStrLn "Please enter either 'from' or 'to' to convert from or to morse."
                --putStrLn "Wrong input!"
                exitFailure