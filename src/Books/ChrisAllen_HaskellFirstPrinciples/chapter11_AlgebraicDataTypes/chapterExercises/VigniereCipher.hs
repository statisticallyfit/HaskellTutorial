import Test.QuickCheck
import Data.Char
import Data.List


-- turns a char between 'a' to 'z' into int from 0 to 25
lowToInt :: Char -> Int
lowToInt letter = ord letter - ord 'a'

--- turns char between 'A' to 'Z' into int from 0 to 25
uppToInt :: Char -> Int
uppToInt letter = ord letter - ord 'A'

-- turns a num between 0 to 25 into char from 'a' to 'z'
intToLow :: Int -> Char
intToLow n = chr (ord 'a' + n)

-- turns num from 0 to 25 into char from 'A' to 'Z'
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

type Text = String
type Keyword = String
type Shift = Int

-- Vigniere cipher
-- 1. get keyword. Repeat it. Take amount equal to length of text. Make text all uppercase.
-- 2. get shift of each letter in keyword with respect to 'A'. Make keywd all uppercase.
-- 3. map the text over the shift list with shift function to get shifted result.

putSpaces :: [Shift] -> Keyword -> Keyword
putSpaces [] xs = xs
putSpaces (p:ps) xs = newPart ++ " " ++ putSpaces ps' rest
    where (newPart, rest) = splitAt p xs
          ps' = map (\pos -> pos - (p + 1)) ps

-- note ignores chars like \n or \t
getSpaces :: Text -> [Shift]
getSpaces {-text-} = elemIndices ' ' {-text-}

-- note ignores chars like \n or \t - leaves them in.
removeSpaces :: Text -> Text
removeSpaces {-text-} = filter (/= ' ') {-text-}



-- note gets the shift difference between two letters
-- note the chars can come in any order. So getShift 'm' 'g' == getShift 'g' 'm'
getShift :: (Char, Char) -> Shift
getShift (a,b) = abs (ord a - ord b)

-- gets input from textKeyPairs - gets the shift difference of each pair.
-- postcondition: returns list of ints which can be greater than 25. For example, if there
-- was a space then getShift 'A' ' ' is 33
getAllShifts :: [(Char, Char)] -> [Shift]
getAllShifts pairs = map getShift pairs


-- note
-- 1. remove space from text and take that length amount of key repeated.
-- 2. in new key, put spaces at same spots as in text.
-- postcondition: keyword result is always in uppercase.
makeKeyword :: Text -> Keyword -> Keyword
makeKeyword text keyStart = keySpaced
    where textNoSpace = removeSpaces $ map toUpper text
          keyCycled = take (length textNoSpace) (map toUpper $ concat $ repeat keyStart)
          keySpaced = putSpaces (getSpaces text) keyCycled

-- given the keyword from above, return the shift of each letter relative to 'A'.
 -- precondition uppercase keyword.
getKeyShifts :: Keyword -> [Shift]
getKeyShifts key = getAllShifts pairs
    where pairs = zip key (repeat 'A')



-- note takes shift amounts and shifts the original text
encodeVigniere :: Text -> Keyword -> Text
encodeVigniere text key = map convert paired
    where keyword = makeKeyword text key
          shifts = getKeyShifts keyword
          paired = zip text shifts
          between x l u = x >= l && x <= u -- inclusive
          convert (char, factor)
            | char == ' ' || (not $ between factor 0 25) = char
            | otherwise = shift factor char





--- TESTING ------------------------------------------------------------------------------

--- HELP these tests fail (some) because of improper input like "\249"

testPutGetSpacing :: String -> Bool
testPutGetSpacing text = (putSpaces ps textNoSpace) == text
    where ps = getSpaces text
          textNoSpace = removeSpaces text

testNoShift :: Char -> Bool
testNoShift c = getShift (c,c) == 0

testShiftOne :: Char -> Bool
testShiftOne c = getShift (c, (chr (ord c + 1))) == 1

testShiftSwitch :: Char -> Char -> Bool
testShiftSwitch c1 c2 = getShift (c1,c2) == getShift (c2,c1)


--- testing that where uppercase and lowercase in given text, so is the result.
testEncodeV :: Text -> Keyword -> Bool
testEncodeV text key = lengthPreserved && casePreserved
    where code = encodeVigniere text key
          pairs = zip code text
          lengthPreserved = length code == length text
          casePreserved = and $ map (\(c,t) -> isUpper c && isUpper t
                                            || isLower c && isLower t) pairs

testWrapAroundVigniere :: Bool
testWrapAroundVigniere = "MPPR AE OYWYH" == encodeVigniere "MEET AT DAWNW" "ally"