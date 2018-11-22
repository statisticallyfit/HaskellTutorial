import Prelude hiding (lines, Word)
import Data.List hiding (lines)


type Doc  = String -- the given text.
type Line = String
type Word = String
type Range = String -- example: "5-7"


whitespace :: String
whitespace = " \n\t;:.,\'\"!?()-"


-- exercise 30 ----------------------------------------------------------------------
-- note splits a string into list of lines at \n
-- precondition: assume it doesn't start or end with \n character.
lines :: Doc -> [Line]
lines [] = []
lines text = takeWhile (/= '\n') text : lines restNoNewline
    where rest = dropWhile (/= '\n') text
          restNoNewline = if (take 1 rest == "\n") then dropWhile (== '\n') rest
                          else rest
-------------------------------------------------------------------------------------


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
-- note the same pair is not considered in order, so sortLs remvoes duplictes.
orderPair :: (Int, Word) -> (Int, Word) -> Bool
orderPair (n1, w1) (n2, w2) = w1 < w2 || (w1 == w2 && n1 < n2)


-- note sorts (int, word) tuple pairs as determined by orderPair function.
-- postcondition the same pair is not  in order, so sortLs removes duplicates.
sortLs :: [(Int, Word)] -> [(Int, Word)]
sortLs [] = []
sortLs (p:ps) = sortLs smaller ++ [p] ++ sortLs larger
    where smaller = [q | q <- ps, orderPair q p] -- all pairs smaller than p
          larger  = [q | q <- ps, orderPair p q] -- all pairs larger than p



-- exercise 31 ---------------------------------------------------------------------
-- note makes each int the the pair into a list, ready for amalgamation
makeLists :: [(Int, Word)] -> [([Int], Word)]
makeLists = map (\(n,st) -> ([n], st)) --mklis
    --where mklis (n, st) = ([n], st)


-- note removes words less than 4 letters long
shorten :: [([Int], Word)] -> [([Int], Word)]
shorten = filter (\(n, w) -> length w > 3) -- instead of sizer works too.
    --where sizer (n, w) = length w > 3



makeLists' :: [(Int, Word)] -> [([Int], Word)]
makeLists' ps = [mklis p | p <- ps]
    where mklis (n, st) = ([n], st)

shorten' :: [([Int], Word)] -> [([Int], Word)]
shorten' ps = [p | p <- ps, sizer p]
    where sizer (n, w) = length w > 3

------------------------------------------------------------------------------------


-- note combines lists for the same word
-- precondition: the words are in sorted order so this method of checking
-- next word works.
amalgamate :: [([Int], Word)] -> [([Int], Word)]
amalgamate [] = []
amalgamate [p] = [p]
amalgamate ((l1, w1) : (l2, w2) : rest)
    | w1 /= w2  = (l1, w1) : amalgamate ((l2, w2) : rest)
    | otherwise = amalgamate ((l1 ++ l2, w1) : rest)



-- exercise 32 -------------------------------------------------------------------

connect :: (Eq a, Num a) => [[a]] -> [[a]]
connect [p] = [p]
connect (p1:p2:ps)
    | last p1 + 1 == head p2 = connect ([p1 ++ p2] ++ ps)
    | otherwise              = [p1] ++ connect (p2:ps)


liftConsec :: (Eq a, Num a) => [a] -> [[a]]
liftConsec xs = connect paired
    where paired = groupBy (\a b -> b-a == 1) xs


makeRanges :: [[Int]] -> [Range]
makeRanges xss = map makeRange xss
    where makeRange xs = if length xs == 1
                         then show (head xs)
                         else show (head xs) ++ "-" ++ show (last xs)



ranges :: [([Int], Word)] -> [([Range], Word)]
ranges pairs = zip allRanges allWords
    where allNumLists = map (\(ns, w) -> ns) pairs
          allWords = map (\(ns, w) -> w) pairs
          allRanges = map makeRanges (map liftConsec allNumLists)








makeIndex :: Doc -> [([Range], Word)]
makeIndex = ranges      .
            shorten     .
            amalgamate  .
            makeLists   .
            sortLs      .
            allNumWords .
            numLines    .
            lines -- this comes first

doc = "cathedral bedragged\n\
     \ hammer loki thor\ncathedral loki cat\nsummer sword\
     \ asgard\ncathedral\nbedragged swinging on a\
     \ hammock\nswinging\ncats and dogs cats loki\n"

doc2 = "cathedral bedragged\n\
              \ cathedral hammer locked\n\
              \ cathedral open hammer bedragged\n\
              \ swim bedragged locked\n\
              \ cathedral\n\
              \ swim bedragged hammer\n\
              \ cathedral swim locked hammer\n\
              \ cathedral open open swim\n"



