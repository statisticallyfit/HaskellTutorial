import Test.QuickCheck
import Data.Char
import Data.List


-- turns a char between 'a' to 'z' into int from 1 to 26
lowToInt :: Char -> Int
lowToInt letter = ord letter - ord 'a'

--- turns char between 'A' to 'Z' into int from 1 to 26
uppToInt :: Char -> Int
uppToInt letter = ord letter - ord 'A'

-- turns a num between 1 to 26 into char from 'a' to 'z'
intToLow :: Int -> Char
intToLow n = chr (ord 'a' + n)

-- turns num from 1 to 26 into char from 'A' to 'Z'
intToUpp :: Int -> Char
intToUpp n = chr (ord 'A' + n)


-- note: if char is not an alpha then it just returns it.
-- else it shifts it.
shift :: Int -> Char -> Char
shift factor c
    | isLower c = intToLow ((lowToInt c + factor) `mod` 26)
    | isUpper c = intToUpp ((uppToInt c + factor) `mod` 26)
    | otherwise = c


encodeCaesar :: Int -> String -> String
encodeCaesar factor originalText = map (shift factor) originalText


decodeCaesar :: Int -> String -> String
decodeCaesar factor codedText = encodeCaesar (-factor) codedText




------------

-- Vigniere cipher
-- 1. get keyword. Repeat it. Take amount equal to length of text. Make text all uppercase.
-- 2. get shift of each letter in keyword with respect to 'A'. Make keywd all uppercase.
-- 3. map the text over the shift list with shift function to get shifted result.

putSpaces :: [Int] -> String -> String
putSpaces [] xs = xs
putSpaces (p:ps) xs = newPart ++ " " ++ putSpaces ps' rest
    where (newPart, rest) = splitAt p xs
          ps' = map (\pos -> pos - (p + 1)) ps

-- note ignores chars like \n or \t
getSpaces :: String -> [Int]
getSpaces {-text-} = elemIndices ' ' {-text-}

-- note ignores chars like \n or \t - leaves them in.
removeSpaces :: String -> String
removeSpaces {-text-} = filter (/= ' ') {-text-}


-- note
-- 1. remove space from text and take that length amount of key repeated.
-- 2. then put the spaces in original text at space spots.
-- 3. zip original text and spaced keyword from step 2.
textKeyPairs :: String -> String -> [(Char, Char)]
textKeyPairs text keyword = zip text keySpaced
    where textNoSpace = removeSpaces text
          keyCycled = take (length textNoSpace) (concat $ repeat keyword)
          keySpaced = putSpaces (getSpaces text) keyCycled


-- note gets the shift difference between two letters
-- note the chars can come in any order. So getShift 'm' 'g' == getShift 'g' 'm'
getShift :: Char -> Char -> Int
getShift a b = abs (ord a - ord b)

-- gets input from textKeyPairs - gets the shift difference of each pair.
getAllShifts :: [(Char, Char)] -> [Int]
getAllShifts pairs = map (\(t,k) -> getShift t k) pairs







--- TESTING ------------------------------------------------------------------------------

testPutGetSpacing :: String -> Bool
testPutGetSpacing text = (putSpaces ps textNoSpace) == text
    where ps = getSpaces text
          textNoSpace = removeSpaces text

testNoShift :: Char -> Bool
testNoShift c = getShift c c == 0

testShiftOne :: Char -> Bool
testShiftOne c = getShift c (chr (ord c + 1)) == 1

testShiftSwitch :: Char -> Char -> Bool
testShiftSwitch c1 c2 = getShift c1 c2 == getShift c2 c1