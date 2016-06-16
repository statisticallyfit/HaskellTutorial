import Prelude hiding (lines, Word)
import qualified Prelude
--import Data.List hiding (lines)


type Doc  = String -- the given text.
type Line = String
type Word = String
type Range = String -- example: "5-7"


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




-- exercise 38 ---------------------------------------------------------------------
-- note applyes previous function to the whole text AND shortens (ex 38_
allNumWords :: [(Int, Line)] -> [(Int, Word)]
allNumWords = shorten . concat . map numWords -- list of int-line pairs here as arg
    where shorten = filter (\(n, w) -> length w > 3)

-----------------------------------------------------------------------------------





-- note tests if a pair is in order.
-- 1. first word must come before second
-- 2. or words can be equal and first num must be less than second.
-- In other words: for pairs containing same word, ordering is by line number.
-- note the same pair is not considered in order, so sortLs remvoes duplictes.
orderPair :: (Int, Word) -> (Int, Word) -> Bool
orderPair (n1, w1) (n2, w2) = w1 < w2 || (w1 == w2 && n1 < n2)







-- exercise 33 --------------------------------------------------------------------

-- note allows duplicates
lessEqual :: (Int, Word) -> (Int, Word) -> Bool
lessEqual (n1, w1) (n2, w2) = w1 < w2 || (w1 == w2 && n1 <= n2)

greater :: (Int, Word) -> (Int, Word) -> Bool
greater (n1, w1) (n2, w2) = w1 > w2 || (w1 == w2 && n1 > n2)

-- note allows duplicates
sortD :: [(Int, Word)] -> [(Int, Word)]
sortD [] = []
sortD (p:ps) = sortD smaller ++ [p] ++ sortD larger
    where smaller = [q | q <- ps, greater p q] -- all pairs smaller than p
          larger  = [q | q <- ps, lessEqual p q] -- all pairs larger than p


-- TEST THAT lessEqual and greater are mutually exclusive

ps1 = [(2,"a"),(2,"b"),(2,"c"),  (1,"a"),(1,"b"),(1,"c"),  (1,"a"),(1,"b"),(1,"c")]
ps2 = [(1,"b"),(1,"a"),(1,"c"),  (1,"b"),(1,"a"),(1,"c"),  (2,"b"),(2,"a"),(2,"c")]
ps = zip ps1 ps2

evalLessPair = map (\(p1,p2) -> lessEqual p1 p2) ps
evalGreaterPair = map (\(p1,p2) -> lessEqual p1 p2) ps

------------------------------------------------------------------------------------







-- note sorts (int, word) tuple pairs as determined by orderPair function.
-- postcondition the same pair is not  in order, so sortLs removes duplicates.
sortLs :: [(Int, Word)] -> [(Int, Word)]
sortLs [] = []
sortLs (p:ps) = sortLs smaller ++ [p] ++ sortLs larger
    where smaller = [q | q <- ps, orderPair q p] -- all pairs smaller than p
          larger  = [q | q <- ps, orderPair p q] -- all pairs larger than p



-- note makes each int the the pair into a list, ready for amalgamation
makeLists :: [(Int, Word)] -> [([Int], Word)]
makeLists = map (\(n,st) -> ([n], st)) --mklis
    --where mklis (n, st) = ([n], st)

{-

-- note removes words less than 4 letters long
shorten :: [([Int], Word)] -> [([Int], Word)]
shorten = filter (\(n, w) -> length w > 3) -- instead of sizer works too.
    --where sizer (n, w) = length w > 3
-}






-- exercise 34 ---------------------------------------------------------------------
getUntil :: (a -> Bool) -> [a] -> [a]
getUntil p []   = []
getUntil p (x:xs)
    | p x       = []
    | otherwise = x : getUntil p xs

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p [] = []
dropUntil p (x:xs)
    | p x = (x:xs)
    | otherwise = dropUntil p xs



amalgamate' :: [([Int], Word)] -> [([Int], Word)]
amalgamate' [] = []
amalgamate' ps = (allNums, firstWord) : amalgamate' rest
    where firstWord = snd $ head ps
          batch = getUntil (\(n,w) -> w /= firstWord) ps
          allNums = map (\([n], w) -> n) batch
          rest = dropUntil (\(n,w) -> w /= firstWord) ps


-----------------------------------------------------------------------------------



-- note combines lists for the same word
-- precondition: the words are in sorted order so this method of checking
-- next word works.
amalgamate :: [([Int], Word)] -> [([Int], Word)]
amalgamate [] = []
amalgamate [p] = [p]
amalgamate ((l1, w1) : (l2, w2) : rest)
    | w1 /= w2  = (l1, w1) : amalgamate ((l2, w2) : rest)
    | otherwise = amalgamate ((l1 ++ l2, w1) : rest)




makeIndexD :: Doc -> [([Int], Word)]
makeIndexD = {-shorten     .-}
             amalgamate  .
             makeLists   .
             sortD       .
             allNumWords .
             numLines    .
             lines


makeIndex :: Doc -> [([Int], Word)]
makeIndex = {-shorten     .-}
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
      \ cathedral open hammer open open bedragged\n\
      \ swim bedragged locked\n\
      \ cathedral cathedral swim swim swim bedragged bedragged\n\
      \ swim bedragged hammer\n\
      \ cathedral swim swim swim locked hammer\n\
      \ cathedral open open swim\n"



-- note first int is length the entire word plus space should be.
showLine :: Int -> ([Int], Word) -> String
showLine n (ns, word) = word ++ space ++ nums ++ "\n"
    where space = replicate (n - (length word) + 3) ' ' -- note +3 for extra space
          nums = reverse $ tail $ reverse $ tail list
          list = show ns


showIndex :: [([Int], Word)] -> String
showIndex ps = titleWord ++ extraSpace ++ titleLines ++ lines ++ "\n"
    where maxWordLen = maximum $ map (\(ns,w) -> length w) ps
          titleWord = "\nWord:" ++ (replicate (maxWordLen - 5) ' ')
          titleLines = "Line Numbers:" ++ (replicate (maxWordLen - 13) ' ') ++ "\n"
          lines = concat $ map (showLine maxWordLen) ps
          extraSpace = replicate 3 ' '

printIndex :: [([Int], Word)] -> IO()
printIndex ps = putStr $ showIndex ps





main = print $ makeIndexD doc2
