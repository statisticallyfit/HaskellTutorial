import Prelude hiding (Word, getLine)


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


-- exercise 27 -------------------------------------------------------------------------
dropLine :: Int -> [Word] -> Line
dropLine len []       = []
dropLine len (w:ws)
    | length w <= len = dropLine newLen ws
    | otherwise       = (w:ws)
    where newLen = len - (length w + 1)
----------------------------------------------------------------------------------------


-- note n = line length
{-splitLines :: Int -> [Word] -> [Line]
splitLines _ [] = []
splitLines n ws = getLine n ws : splitLines n (dropLine n ws)-}

splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines ws = getLine n ws : splitLines (dropLine n ws)
                where n = 20



-- note could have used joinLines
printLines :: [Line] -> IO()
printLines lines = putStrLn $ concat ["\t" ++ joinLine line ++ "\n" | line <- lines]


-- note fills a text string into lines
fill :: Text -> [Line]
fill = splitLines . splitWords



line = ["12", "345", "67"]
maryLittleLamb  = ["Mary","had","a","little","lamb","his","fleece","was",
                   "white","as","snow.","And","everywhere","that","Mary","went","the","lamb",
                   "was","sure","to","go."]

blackSheepString  = "Bahh bahh black sheep have you any wool? Yes sir, yes sir, \
                   \ three bags full. One for my master, one for the maid, \
                   \ and one for the little boy who lives down the main. \
                   \ Bahh bahh black sheep have you any wool? Yes sir, yes sir, \
                   \ three bags full!"

blackSheepLines = fill blackSheepString



-- exercise 28 ------------------------------------------------------------------------
joinLine :: Line -> String
joinLine []     = []
joinLine (w:ws) = w ++ " " ++ joinLine ws

-- exercise 29 ------------------------------------------------------------------------
joinLines :: [Line] -> String
joinLines []     = []
joinLines (l:ls) = joinLine l ++ "\n" ++ joinLines ls


main = do
    print $ dropLine 0 line
    print $ dropLine 1 line
    print $ dropLine 2 line
    print $ dropLine 3 line
    print $ dropLine 4 line
    print $ dropLine 5 line
    print $ dropLine 6 line
    putStrLn "" ----------------------------------------------------------------------
    print $ splitWords "  dog cat"
    print $ getLine 20 ["Mary", "Poppins", "looks", "like"]
    print $ dropLine 20 ["Mary", "Poppins", "looks", "like"]
    putStrLn $ joinLine ["Mary","had","a"]
    --print $ splitLines 15 poem
    --printLines $ splitLines 15 poem
    printLines $ splitLines maryLittleLamb
    putStrLn "" ----------------------------------------------------------------------
    printLines $ fill "Bahh bahh black sheep have you any wool? Yes sir, yes sir, \
                       \ three bags full. One for my master, one for the maid, \
                       \ and one for the little boy who lives down the main. \
                       \ Bahh bahh black sheep have you any wool? Yes sir, yes sir, \
                       \ three bags full!"
    putStrLn ""
    print $ joinLines blackSheepLines
