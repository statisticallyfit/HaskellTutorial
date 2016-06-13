import Prelude hiding (Word)

type Text = String
type Word = String
type Line = [Word]


whitespace = ['\n', '\t', ' ']

isWhitespace :: Char -> Bool
isWhitespace c = elem c whitespace

isNotWhitespace :: Char -> Bool
isNotWhitespace c = not (isWhitespace c)

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p [] = []
dropUntil p (x:xs)
    | p x = (x:xs)
    | otherwise = dropUntil p xs

getUntil :: (a -> Bool) -> [a] -> [a]
getUntil p []   = []
getUntil p (x:xs)
    | p x       = []
    | otherwise = x : getUntil p xs


------------------------------------------------------------------------------------

-- NOTE had major help for the answer here.

splitLines :: Text -> Int -> [Line]
splitLines text lineLen = splitLines' text [] lineLen 0

splitLines' :: Text -> Line -> Int -> Int -> [Line]
splitLines' [] lineAcc _ _ = [lineAcc]
splitLines' text lineAcc lineLen lenAcc
    | lenAcc + length word <= lineLen
        = splitLines' rest (lineAcc ++ [word]) lineLen (lenAcc + length word + 1)
    | otherwise = lineAcc : splitLines' text [] lineLen 0
    where word = getUntil isWhitespace text
          rest = dropUntil isNotWhitespace (dropUntil isWhitespace text)
