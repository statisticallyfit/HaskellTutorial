module VigniereCipher where

import Data.Char
import Data.List

import CaesarCipher



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
replicateKeyWord :: Text -> Keyword -> Keyword
replicateKeyWord text keyStart = keySpaced
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
    where keyword = replicateKeyWord text key
          shifts = getKeyShifts keyword
          paired = zip text shifts
          between x l u = x >= l && x <= u -- inclusive
          convert (char, factor)
            | char == ' ' || (not $ between factor 0 25) = char
            | otherwise = shift factor char
