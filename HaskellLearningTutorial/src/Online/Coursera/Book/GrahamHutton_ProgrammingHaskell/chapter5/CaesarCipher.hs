import Data.Char
--import Prelude

-- A code that to be broken requires shifting all the letters in the
-- code by a factor
-- SO "haskell is fun" is written as:  "kdvnhoo lv ixq" with shift factor = 3




count      :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

--lowers    :: String -> Int
--lowers xs = length [x | x <- xs, isLower x]
letters   :: String -> Int
letters xs = length [x | x <- xs, isAlpha x]



-- turns a char between 'a' and 'z' into an integer betwen 0 and 25
lowToInt :: Char -> Int
lowToInt c = ord c - ord 'a'

uppToInt :: Char -> Int
uppToInt c = ord c - ord 'A'

-- turns an int between 0 and 25 into a char between 'a' and 'z'
intToLow :: Int -> Char
intToLow n = chr (ord 'a' + n)

intToUpp :: Int -> Char
intToUpp n = chr (ord 'A' + n)


shift           :: Int -> Char -> Char
shift factor c
    | isLower c = intToLow ((lowToInt c + factor) `mod` 26)
    | isUpper c = intToUpp ((uppToInt c + factor) `mod` 26)
    | otherwise = c


encode     :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

decode      :: Int -> String -> String
decode n xs = encode (-n) xs



-- Cracking the cipher (frequency tables)
-- table of expected frequencies of english language letters
table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2, 6.1, 7, 0.2, 0.8, 4, 2.4,
         6.7, 7.5, 1.9, 0.1, 6, 6.3, 9.1, 2.8, 1, 2.4, 0.2, 2, 0.1]


percent    :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100


freqs   :: String -> [Float]
freqs xs = [percent (count x xs') n | x <- ['a' .. 'z']]
            where n   = letters xs -- the count of lower-case given letters
                  xs' = map toLower xs

-- comparing a list of observed frequencies 'os' with a list of
-- expected frequencies 'es' is the chi-square statistic
chiSquare       :: [Float] -> [Float] -> Float
chiSquare os es = sum [ ( (o - e)^2) / e | (o, e) <- zip os es]

-- rotate list n places to the list, wrapping around at the start of the list
-- assume that 0 <= n <= list length
rotate      :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs


positions      :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                 where n = length xs - 1

crack   :: String -> String
crack xs = decode factor xs
           where
                factor = head (positions (minimum chiTable) chiTable)
                chiTable = [chiSquare (rotate n table') table | n <- [0 .. 25]]
                table' = freqs xs



main = do
    print $ lowToInt 'a'; print $ lowToInt 'k'; print $ lowToInt 'z'
    print $ intToLow 0; print $ intToLow 10; print $ intToLow 25
    print $ shift 2 'c'; print $ shift 4 'z'; print $ shift (-3) 'c'
    print $ shift 1 'A'; print $ shift 3 ' '
    putStrLn ""
    print ( encode 3 "haskell is fun")
    print ( decode 3 "kdvnhoo lv ixq")
    print (freqs "abbcccddddeeeee")
    putStrLn ""
    print $ crack "kdvnhoo lv ixq"
    print $ crack "vscd mywzboroxcsyxc kbo ecopev"
    -- it may not work if the word has unusual distribution of letters or is short
    print $ crack (encode 3 "haskell")
    print $ crack (encode 3 "boxing wizards jump quickly")
    print $ crack (encode 3 "ana the banana")
    print $ crack (encode 3 "functional programming is awesome")
    -------------------------------------------------------------- exercise 8 for lower and uppercase
    print $ freqs "AAaAbbBbcccdddddDeeeEeeeE"
    print $ crack "vscd mywzbOROxcsYxC kBo EcoPEV"
    print $ crack "vscd mywzbOROxcsYxC kBo EcoPEV 123"
    print $ crack (encode 3 "Functional Programming Is Awesome. Today is May 5th")