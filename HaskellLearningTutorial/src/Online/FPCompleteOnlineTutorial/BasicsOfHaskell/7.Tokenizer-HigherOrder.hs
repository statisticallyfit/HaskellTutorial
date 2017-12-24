import Data.Char
import Prelude hiding (span)


data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokIdent String
           | TokNum Int
           | TokSpace
    deriving (Show, Eq)


-- will only take a token
deSpace :: [Token] -> [Token]
deSpace = filter (\t -> t /= TokSpace)
--deSpace = filter notSpace

--notSpace :: Token -> Bool
--notSpace t = t /= TokSpace




-- Exercise 1 --------------------------------------------------------------------
toInts :: String -> [Int]
toInts = map digitToInt

-- Exercise 2 --------------------------------------------------------------------

squares :: [Int] -> [Int]
squares = map (^2)
-- squares = map (\x -> x^2)

-- Exercise 3 --------------------------------------------------------------------
type Point = (Double, Double)
inCircle2D :: [Point] -> [Point]
inCircle2D = filter (\(x,y) -> sqrt (x^2 + y^2) <= 2.0)

----------------------------------------------------------------------------------


{-}
-- TOKENIZING IDENTIFIERS: Now going to tokenize multi-character tokens
tokenize (c:cs)
    | isAlpha c = identifier c cs


-- identifier takes the already recognized c plus the rest cs for further processing
identifier c cs = let (str, cs') = alphanums cs
                  in TokIdent (c:str) : tokenize cs'
-- note: this is mutual recursion - multiple functions recurse into each other.


-}
-- alphanums accumulates alphanumeric characters.
-- returns the leftover input together with the recognized characters.
alphanums :: String -> (String, String)
alphanums str = span isAlpha str
{-}
alphanums str = als "" str
                where
                    als acc [] = (acc, []) -- acc is the accumulator
                    als acc (c:cs)
                        | isAlpha c = let (acc', cs') = als acc cs
                                      in (c:acc', cs')
                        | otherwise = als acc cs
                            -- note: was otherwise = (acc, c:cs) but this stopped
                            -- at the first wrong character, so I fixed it above.
-}
-- note: let is local to each body in the function but where is visible
-- across all bodies.



{-}Traversal with accumulation is also a very common pattern and is encapsulated
 in foldl and foldr (fold left and fold right) -- higher order functions
 defined in the Prelude. For instance, here's the signature of foldl:

foldl :: (a -> b -> a) -> a -> [b] -> a
a is the type of the accumulator and [b] is the input list type.
foldl traverses the list from left to right, calling the function (a -> b -> a)
with two arguments: the current accumulator and the current element.
The function returns the new accumulator, which is then used in the next iteration.

Ex 4. Use foldl to calculate the sum of squares given a list of doubles.
-}

-- Exercise 4 --------------------------------------------------------------------
-- sum of squares
sumSquares :: [Int] -> Int
sumSquares = foldl (\acc x -> acc + x^2 ) 0


-- Exercise 5 ---------------------------------------------------------------------
rev :: [a] -> [a] -- acu is accumulator = note: 2:1 foldl [3,4,5] and x = 3
rev = foldl (\acu x -> x:acu) []

rev' = foldl (flip (:)) []

-- Exercise 6 ---------------------------------------------------------------------
type Accum = (Bool, String, String)


-- help: todo: understand better
alphanums' :: String -> (String, String)
alphanums' str = let (_, als, rest) = foldl f (True, [], []) str
                 in (als, rest)
           where
                f (True, als, rest) c   | isAlphaNum c = (True, als ++ [c], rest)
                                        | otherwise    = (False, als, [c])
                f (False, als, rest) c = (False, als, rest ++ [c])





-- TOKENIZING NUMBERS -------------------------------------------------------------

tokenize (c:cs)
    | isDigit c = number c cs

number c cs = let (digs, cs') = digits cs in
              TokNum (read (c : digs)) : tokenize cs'

-- accumulates digits (numbers) rather than alphanumerics
digits :: String -> (String, String)
digits str = span isDigit str
{-}digits :: String -> (String, String)
digits str = digs "" str
    where
        digs :: String -> String -> (String, String)
        digs acc [] = (acc, [])
        digs acc (c:cs) | isDigit c =
                                let (acc', cs') = digs acc cs
                                in (c:acc', cs')
                        | otherwise = digs acc cs
                          --note: fixed it was "otherwise = (acc, c:cs)" before
-}
-- refactor the pattern out of "digits" and "alphanums" functions

span :: (a -> Bool) -> [a] -> ([a], [a])
span pred str = spanAcc [] str
    where  -- define he.lper 'spanAcc"
        spanAcc acc []     = (acc, [])
        spanAcc acc (c:cs) | pred c = (c:acc', cs')
                           | otherwise = spanAcc acc cs
                           where (acc', cs') = spanAcc acc cs
                           -- note: fixed it was "otherwise = (acc, c:cs)" before


main = do
    putStrLn $ filter isDigit "1x + 3y"
    print $ toInts "1234"
    print $ squares [1..10]
    print $ inCircle2D [(0, 0), (2, -2), (1, -1), (1.9, 0.1), (10, 1)]
    ---------------------------------------------------------------------
    print $ alphanums "R2D2+C3Po"
    print $ alphanums' "R2D2+C3Po" -- help how is this output correct?
    print $ digits "R2D2+C3Po"
    ---------------------------------------------------------------------
    print $ sumSquares [1..5]
    print $ rev [1..5]; print $ rev "spot on"
    print $ rev "docnoteidissentafastneverpreventsafatnessidietoncod"
    print $ rev' [1..5]