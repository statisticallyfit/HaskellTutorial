import Data.Time
import System.IO.Unsafe
import Prelude hiding ((<*>), lines, Word)
import qualified Prelude


type Picture = [[Char]]


flipH :: Picture -> Picture
flipH = reverse


flipV :: Picture -> Picture
flipV = map reverse

above :: Picture -> Picture -> Picture
above = (++)

beside :: Picture -> Picture -> Picture
beside = zipWith (++)

-- note map to each string the function (map invertChar) which means map to each
-- char and invert it.
invertColour :: Picture -> Picture
invertColour = map (map invertChar)
    where invertChar c = if c == '.' then '#' else '.'

-- note zip each string with function (zipWith combineChar) which means zip each
-- char in each string with combine function.
superimpose :: Picture -> Picture -> Picture
superimpose = zipWith (zipWith combineChar)
    where combineChar a b = if a == b && a == '.' then '.' else '#'



draw :: Picture -> IO()
draw = putStr . concat . map (++ "\n")
-- same as: putStr $ concat $ map ( ++ "\n") pic



p1 = ["##########", "######....", "###.......", "##........", "#........."]
p2 = ["##########", "###....###", "###....###", "###....###", "##########"]
p  = ["...####...", "...#..#...", "...####...", "...#......", "...#......"]
b  = ["...####...", "...#...#..", "...####...", "...#...#..", "...####..."]
l  = ["..#.......", "..#.......", "..#.......", "..#.......", "..######.."]
n  = ["..#....#..", "..#.#..#..", "..#..#.#..", "..#...##..", "..#....#.."]








-- 12.2 STRATEGY COMBINATORS -------------------------------------------------------

data Move = Rock | Paper | Scissors deriving (Eq, Show)

type Strategy = [Move] -> Move


alternate :: Strategy -> Strategy -> Strategy
alternate str1 str2 moves =
    case length moves `rem` 2 of
        1 -> str1 moves
        0 -> str2 moves

alternate' str1 str2 =
    \moves -> case length moves `rem` 2 of
                1 -> str1 moves
                0 -> str2 moves


alternate'' str1 str2 moves =
    map ($ moves) [str1, str2] !! (length moves `rem` 2)


{-

beatStrategy :: Strategy -> Strategy
beatStrategy opponentStrat moves = beat (opponentStrat moves)
-}










-- 12.3 REGULAR EXPRESSIONS ------------------------------------------------------

-- epsilon - empty string
-- x = pattern x
-- (r1 | r2) = string st will match this if st matches either r1 or r2 or both
-- (r1r2) = string st will match this if st can be split into two substrings
--      where st = st1 ++ st2 so that st1 matches r1 and st2 matches r2
-- (r)* = string st will match this if st can be split into zero or more substrings
--      where st = st1 ++ st2 ++ .. ++ stn each of which matches r. The zero case
--      means that empty string will match (r)* for any regular expression r.

type RegExp = String -> Bool

epsilon :: RegExp
epsilon = (== "")

char :: Char -> RegExp
char ch = (== [ch])

(|||) :: RegExp -> RegExp -> RegExp
e1 ||| e2 = \x -> e1 x || e2 x


-- note splits ("Spy") means:
-- ==> [("", "Spy"), ("S", "py"), ("Sp", "y"), ("Spy", "")]
-- note run through all splits; test whether both regexpressions match
-- the patterns. Need both matches to succeed (and). Then tell if there was any
-- success in the list (or).
-- HELP understand evaluation
(<*>) :: RegExp -> RegExp -> RegExp
e1 <*> e2 = \x -> or [e1 y && e2 z | (y, z) <- splits x ]

-- HELP understand evaluation
(<**>) :: RegExp -> RegExp -> RegExp
e1 <**> e2 = \x -> or [e1 y && e2 z | (y, z) <- tail $ splits x ]

-- note means to match (p)*: either it matches zero times (epsilon) or
-- match p followed by (p)*.
-- HELP understand how this doesn't go in infinite loop
star :: RegExp -> RegExp
star p = epsilon ||| (p <**> star p)
-- using <*> may give infinite recursion because first pattern ("","word")
-- will match.




-- note splits "Spy" ===> [("", "Spy"), ("S", "py"), ("Sp", "y"), ("Spy", "")]
splits :: [a] -> [([a], [a])]
splits xs = [ splitAt n xs | n <- [0 .. (length xs)]]



{-
uncover
main = do
    print $ (epsilon ||| epsilon) ""
    print $ (epsilon ||| epsilon) "hi"
    print $ ((char 'a') <*> (char ' ')) "a "
    -- note True because the split ("a", "b") exists.
    print $ ((char 'a') <*> (char 'b')) "ab"
    print $ ((char 'a') <**> (char ' ')) "a "
    -- note False because splits are ===> [(" ","a"),(" a","")]
    print $ ((char 'a') <**> (char ' ')) " a"
-}








-- 12.4. NATURAL NUMBERS --------------------------------------------------------
-- note takes a function takes an "a" and returns an "a"
type Natural a = (a -> a) -> a -> a

-- HELP this works but type definition: Natural Int -> Int does not work!
int :: (Int -> Int) -> Int -> Int -> Int --Natural Int -> Int
int f a b = (f a) + b
--int n = n (+1) 0







-- GRAPHICS AS NUMBERS ----------------------------------------------------------

type Position  = (Int, Int)
type Pixel  = Char
type Bitmap = Position -> Pixel













-- 12.5 EXAMPLE - CREATING AN INDEX ------------------------------------------------

{-
NOTE SPEC:
1. input is a text string separated by newline characters.
2. index gives every line on which the word in question occurs.
3. only words of at least four letters must be indexed.
4. words (with their lines) must be in alphabetical order.

NOTE PROCEDURE:
1. split text (Doc) into lines so we get [Line]
2. pair each line with its line number so we get [(Int, Line)]
3. split lines into words, associating each word with the number of the line on
which it occurs ==> [(Int, Word)]
4. sort this list according to alphabetical order of words.
5. modify lists so that each word is paired with a list containing a single line
number ===> [([Int], Word)].
6. Amalgamate entries for the same word into a list of numbers ==> [([Int], Word)].
7. shorten list by removing all entries for words of less than four letters.
-}



type Doc  = String -- the given text.
type Line = String
type Word = String


whitespace :: String
whitespace = " \n\t;:.,\'\"!?()-"



-- note splits a string into list of lines at \n
-- precondition: assume it doesn't start or end with \n character.
lines :: Doc -> [Line]
lines = Prelude.lines

-- note pairs each line with its line number
numLines :: [Line] -> [(Int, Line)]
numLines ls = zip [1 .. length ls] ls


-- note splits a line into words with line nums attached
numWords :: (Int, Line) -> [(Int, Word)]
numWords (lineNum, line) = map (\word -> (lineNum, word)) (words line)
-- [(lineNum, word) | word <- words line]

-- note applyes previous function to the whole text
allNumWords :: [(Int, Line)] -> [(Int, Word)]
allNumWords = concat . map numWords -- list of int-line pairs here as arg


-- note tests if a pair is in order.
-- 1. first word must come before second
-- 2. or words can be equal and first num must be less than second.
-- In other words: for pairs containing same word, ordering is by line number.
orderPair :: (Int, Word) -> (Int, Word) -> Bool
orderPair (n1, w1) (n2, w2) = w1 < w2 || (w1 == w2 && n1 < n2)


-- note sorts (int, word) tuple pairs as determined by orderPair function.
sortLs :: [(Int, Word)] -> [(Int, Word)]
sortLs [] = []
sortLs (p:ps) = sortLs smaller ++ [p] ++ sortLs larger
    where smaller = [q | q <- ps, orderPair q p] -- all pairs smaller than p
          larger  = [q | q <- ps, orderPair p q] -- all pairs larger than p


-- note makes each int the the pair into a list, ready for amalgamation
makeLists :: [(Int, Word)] -> [([Int], Word)]
makeLists = map mklis
    where mklis (n, st) = ([n], st)

-- note combines lists for the same word
-- precondition: the words are in sorted order so this method of checking
-- next word works.
amalgamate :: [([Int], Word)] -> [([Int], Word)]
amalgamate [] = []
amalgamate [p] = [p]
amalgamate ((l1, w1) : (l2, w2) : rest)
    | w1 /= w2  = (l1, w1) : amalgamate ((l2, w2) : rest)
    | otherwise = amalgamate ((l1 ++ l2, w1) : rest)

-- note removes words less than 4 letters long
shorten :: [([Int], Word)] -> [([Int], Word)]
shorten = filter sizer --(\(n, w) -> length w > 3) instead of sizer works too.
    where sizer (n, w) = length w > 3





makeIndex :: Doc -> [([Int], Word)]
makeIndex = shorten     .
            amalgamate  .
            makeLists   .
            sortLs      .
            allNumWords .
            numLines    .
            lines

doc = "cathedral bedragged\n\
     \ hammer loki thor\ncathedral loki cat\nsummer sword\
     \ asgard\ncathedral\nbedragged swinging on a\
     \ hammock\nswinging\ncats and dogs loki\n"

-- main = print $ makeIndex doc













-- 12.7 UNDERSTANDING PROGRAMS  --------------------------------------------------

mapWhile :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapWhile f p [] = []
mapWhile f p (x:xs)
    | p x       = f x : mapWhile f p xs
    | otherwise = []

eval = mapWhile (2+) (>7) [8,12,7,13,16]


-- mapWhile f p xs            = map f (takeWhile p xs)
-- mapWhile f (const True) xs = map f xs
-- mapWhile id p xs           = takeWhile p xs