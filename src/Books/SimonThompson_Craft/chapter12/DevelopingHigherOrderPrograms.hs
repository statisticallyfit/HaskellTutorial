import Data.Time
import System.IO.Unsafe
import Prelude hiding ((<*>))


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

int :: Natural Int -> Int
int n = n (+1) 0