import Data.List (elemIndex)



-- note
-- precondition sorted list, no duplicates
removeConsec :: [Int] -> [Int]
removeConsec [] = []
removeConsec [x] = [x]
removeConsec [x,y] = if (y - x == 1) then [y] else [x,y]
removeConsec (a:b:c:ds)
    | b - a == 1 && c - b == 1 = removeConsec (c:ds)
    | b - a == 1 && c - b /= 1 = removeConsec (b:c:ds)
    | otherwise                = a : removeConsec (b:c:ds)



-- note returns list of index where all the blanks are located.
getBlanksPositions :: String -> [Int]
getBlanksPositions st = removeConsec [index | index <- [0.. (length st - 1)],
                                                st !! index == ' '] -- counter


-- note inserts one blank at the specified position
--insertBlank 3 "012345"    ==   "012 345"
insertBlank :: Int -> String -> String
insertBlank p str = fst splittedStr ++ " " ++ snd splittedStr
                    where splittedStr = splitAt p str

-- note inserts blanks from pos list consecutively while  length <= lineLen
-- note the length of the string does not have to meet value of n! blankify
-- just makes sure that the length of string does not exceed n. It inserts one additional
-- space between each word assuming spaces are already there.
-- precondition: the blank poslist is reversed! AND the str has at least 1 space between
-- each word.
blankifyLine :: Int -> [Int] -> String -> String
blankifyLine _ [] str    = str
blankifyLine n (p:ps) str
    | length str < n = blankifyLine n ps (insertBlank p str)
    | otherwise      = str



-- note calls blankify repeatedly until the length of the string equals lineLen.
-- compare: blankify goes once through the string, while justify goes until its length
-- equals linelen.
joinJustifyLine :: Int -> String -> String
joinJustifyLine lineLen str
    | length str < lineLen = joinJustifyLine lineLen blankedStr
    | otherwise            = str
    where ps = reverse (getBlanksPositions str) -- work from the back on the forward string
          blankedStr = blankifyLine lineLen ps str



----------------------------------------------------------------------------------------
--- These methods are for testing:


allBlankLengths    :: String -> [Int]
allBlankLengths [] = []
allBlankLengths str
    | blankIsFirst str = blankLength str : allBlankLengths strWordFirst
    | otherwise        = allBlankLengths strBlankFirst
    where blankIsFirst str = if (elemIndex ' ' str == (Just 0)) then True else False
          blankLength str = length $ takeWhile (== ' ') str
          strWordFirst = dropWhile (== ' ') str
          strBlankFirst = dropWhile (/= ' ') str




str = "stellar cosmos in the milky way"
n = length str + 20
ps = reverse $ getBlanksPositions str


main = do
    print $ removeConsec [1,10,12] == [1,10,12]
    print $ removeConsec [2,3,5,10] == [3,5,10]
    print $ removeConsec [2,3,4,10,12] == [4,10,12]
    print $ removeConsec [2,3,4,5,10,12] == [5,10,12]
    print $ removeConsec [2,3,4,5,6,10,12] == [6,10,12]
    print $ removeConsec [2,3,4,5,6] == [6]
    print $ removeConsec [2,3,4,5,6,7,10,13,14,15,16,20] == [7,10,16,20]
    print $ removeConsec [1,3,5,7,8,9,10,11,14,17,18,19,20,30] == [1,3,5,11,14,20,30]
    print $ removeConsec [1,3,5,7,8,9,10,11,14,17,18,19,20] == [1,3,5,11,14,20]
    putStrLn "" ------------------------------------------------------------------------
    print $ not (length (blankifyLine n ps str) == n) -- expr inside not() must be False
    print $ blankifyLine n ps str
    putStrLn "" ------------------------------------------------------------------------
    print $ length (joinJustifyLine n str) == n
    print $ joinJustifyLine n str
    print $ allBlankLengths $ joinJustifyLine n str

