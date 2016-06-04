import Data.Char

--offset :: Int
--offset = ord 'A' - ord 'a'
-- to convert: chr (ord 'c' + offset)


lowerToUpper :: Char -> Char
lowerToUpper c
    | isLower c = toUpper c
    | otherwise = c


capitalize :: String -> String
capitalize str = [lowerToUpper c | c <- str]


capitalizeLetters :: String -> String
capitalizeLetters str = [lowerToUpper c | c <- str, isAlpha c]