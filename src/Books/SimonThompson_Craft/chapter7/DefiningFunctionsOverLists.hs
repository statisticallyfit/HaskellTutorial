import Prelude hiding (sum, concat, zip, take, Word, getLine)
import qualified Prelude
import Test.QuickCheck

sum        :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs


propSum xs = sum xs == Prelude.sum xs

{-
uncover
main = do
    quickCheck propSum-}




-- 7.4 FINDING PRIMITIVE RECURSION FUNCTIONS --------------------------------------------

concat          :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss

propConcat xss = concat xss == (Prelude.concat xss)




iSort        :: [Integer] -> [Integer]
iSort []     = []
iSort (x:xs) = insert x (iSort xs)

-- precondition: the list y:ys is sorted ascendingly
insert :: Integer -> [Integer] -> [Integer]
insert x []     = [x]
insert x (y:ys)
    | y >= x    = x : y : ys
    | otherwise = y : insert x ys

{-
uncover
main = do
    print $ concat [[1,2,3], [4,5]]
    print $ concat ["hi there, ", "how are you?"]
    -- quickCheck propConcat -- help why doesn't print?

-}







-- 7.5 GENERAL RECURSIONS OVER LISTS ----------------------------------------------------

-- NOTE examples of recursion patterns

-- 1
zip :: [a] -> [b] -> [(a,b)]
zip xs []         = []
zip [] ys         = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys


-- 2
take :: Int -> [a] -> [a]
take _ []     = []
take 0 _      = []
take n (x:xs) = x : take (n-1) xs
--take _ _      = error "No negative arg!" -- help where to put this?


 -- 3
qSort :: [Integer] -> [Integer]
qSort [] = []
qSort (x:xs) = qSort [y | y <- xs, y <= x] ++ [x] ++ qSort [y | y <- xs, y > x]













-- 7.6 EXAMPLE: TEXT PROCESSING --------------------------------------------------------

whitespace = ['\n', '\t', ' ']

type Text = String


-- note: stops whenever it encounters a whitespace
-- getWord "  boo" is "" since first char is whitespace
-- getWord "cat dog" is "cat"
-- precondition: in text processing, we expect no whitespace before word.
getWord :: Text -> Text
getWord [] = []
getWord (x:xs)
    | elem x whitespace = [] -- equals if x is a whitespace ' ' char then discontinue
    | otherwise         = x : getWord xs

-- note: if word comes before any whitespace, skip the word and return the rest. Else
-- if there is whitespace first just return what you have.
-- dropWord "    cat dog"    ==    "    cat dog"
-- dropWord "cat  "          == "  "
-- dropWord "cat  dog"       == "  dog"
-- precondition: in text processing, we expect no whitespace before word.
dropWord :: Text -> Text
dropWord [] = []
dropWord (x:xs)
    | elem x whitespace = (x:xs) -- equals if char x is ' ' then return all so far.
    | otherwise         = dropWord xs


-- note
-- dropSpace "  cat"      == "cat"
-- dropSpace "cat dog"    == "cat dog"
-- dropSpace "cat   "     == "cat   "
dropSpace :: Text -> Text
dropSpace [] = []
dropSpace (x:xs)
    | elem x whitespace = dropSpace xs
    | otherwise         = (x:xs)




type Word = String ---------------------------------------------------------------------

split :: Text -> [Word]
split [] = []
split st = (getWord st) : split (dropSpace (dropWord st))


splitWords :: Text -> [Word]
splitWords st = split (dropSpace st)



type Line = [Word] -------------------------------------------------------------------

-- note Int is length of line to be formed
-- Forms a line. Length of patched words must be less than or equal to line length
-- to form the line.
-- cases
--      1. no words means empty line
--      2. if room for word W then it goes on line. Remainder of words must fit under
--         new length of line: len - (length w +1), 1 for the space.
--      3. if first word doesn't fit line must be empty.
getLine               :: Int -> [Word] -> Line
getLine len []        = [] -- case 1
getLine len (w:ws)
    | length w <= len = w : restOfLine
    | otherwise       = []
    where newLen = len - (length w + 1)
          restOfLine = getLine newLen ws







main = do
    print $ getLine 20 ["Mary", "Poppins", "looks", "like"]
