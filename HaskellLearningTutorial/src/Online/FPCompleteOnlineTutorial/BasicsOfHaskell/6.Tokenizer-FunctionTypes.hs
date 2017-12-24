import Data.Char

data Token_ = Digit | Alpha
        deriving (Show, Eq)



tokenize_        :: String -> [Token_]
tokenize_ []     = []
tokenize_ (c:rest)
    | isDigit c = Digit : tokenize_ rest
    | otherwise = Alpha : tokenize_ rest


--main = print $ tokenize "passwd123"

isElem          :: Eq a => a -> [a] -> Bool
isElem _ []     = False
isElem c (d:rest)
    | c == d    = True
    | otherwise = isElem c rest

-- todo: This is curried definition (point-free notation)
-- todo: that expects a list of Char. We are storing this curried function isElem '3'
-- todo: in the variable is3Elem which we can call with a list of Char and get
-- todo: back a Bool.
is3Elem :: [Char] -> Bool
is3Elem = isElem '3'
--is3Elem str = isElem '3' str


data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div
-------------------------------------------------------------------------------
data Token = TokOp Operator
            | TokIdent String
            | TokNum Int
            deriving (Show, Eq)

-- single character tokenization
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | elem c "+-*/" = TokOp (operator c)    : tokenize cs
    | isDigit c     = TokNum (digitToInt c) : tokenize cs
    | isAlpha c     = TokIdent [c]          : tokenize cs
    | otherwise     = error $ "Cannot tokenize " ++ [c]


-- Exercise 3
cat :: [a] -> [a] -> [a]
cat [] ys = ys
cat (x:xs) ys = x : cat xs ys

pig :: String -> String
pig = cat "pig" -- the curried definition -- this is a function that is applied to a string.


-- Exercise 4
toInts :: String -> [Int]
toInts [] = []
toInts (c:cs) = digitToInt c : toInts cs
--toInts cs = map digitToInt cs


-- Exercise 5
sumDig :: String -> Int
sumDig numStr = accum 0 (toInts numStr)
--sum (toInts numStr)

accum          :: Int -> [Int] -> Int
accum a []     = a
accum a (x:xs) = accum (a + x) xs


main = do
    print $ is3Elem "123"
    print $ elem Digit [Digit, Alpha]
    print $ operator '*'
    -------------------------------------------------------
    putStrLn ""
    print $ tokenize "/+-*"
    -------------------------------------------------------
    putStrLn ""
    print $ pig "sty"
    print $ toInts "2013"
    print $ sumDig "2013"