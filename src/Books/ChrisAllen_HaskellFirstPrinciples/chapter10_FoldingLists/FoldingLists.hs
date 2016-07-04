import Prelude hiding (sum, length, product, concat)

{-
NOTE

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)


compare to:
map :: (a -> b) -> [a] -> [b]

-}

sum :: [Integer] -> Integer
sum [] = 0
sum (x:xs) = x + sum xs

length :: [a] -> Integer
length [] = 0
length (_:xs) = 1 + length xs

product :: [Integer] -> Integer
product [] = 1
product (x:xs) = x * product xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs



-- note compare this new definition of foldr with the one above.
foldrNew :: (a -> b -> b) -> b -> [a] -> b
foldrNew f acc xs =
    case xs of
        []     -> acc
        (x:xs) -> f x (foldrNew f acc xs)

{-

NOTE look at the steps using function above.

foldr (+) 0 [1,2,3]

= foldr (+) 0 [1,2,3]
    case [1,2,3] of
        []          -> 0
        (1 : [2,3]) -> (+) 1 (foldr (+) 0 [2,3])

--- note there is (+) 1 implicitly wrapped around this continuation of fold.
--- > foldr (+) 0 [2,3]
        case [2,3] of
            []      -> 0
            (2:[3]) -> (+) 2 (foldr (+) 0 [3])

-- note there is (+) 1 ((+) 2 ...) implicitly wrapped around this continuation of fold.
--- > foldr (+) 0 [3] =
        case [3] of
            []       -> 0
            (3 : []) -> (+) 3 (foldr (+) 0 [])

-- note there is (+) 1 ((+) 2 ((+) 3 ...)) implicitly wrapped around this
-- continuation of fold.
--- > foldr (+) 0 [] =
        case [] of
            [] -> 0 --- equals matched
-}

-- note IMPORTANT seeing how foldr evaluates:
xs = map show [1.. 5]

f :: String -> String -> String
f x y = concat ["(", x, "+", y, ")"]

evalFoldr = foldr f "0" xs