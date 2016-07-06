import Test.QuickCheck
import Data.Char


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


encode :: Int -> String -> String
encode factor originalText = map (shift factor) originalText


decode :: Int -> String -> String
decode factor codedText = encode (-factor) codedText




--- TESTING ------------------------------------------------------------------------------

--- help why does it keep giving up?
testIdentityCode :: Int -> String -> Property
testIdentityCode factor text =
    goodInput ==> (decode factor $ encode factor text) == text
    where goodInput = (length text /= 0) && (and $ map isAscii text)