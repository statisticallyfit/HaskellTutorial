import Prelude hiding (all, any, takeWhile, dropWhile)
import Data.Char

(◦) :: (b -> c) -> (a -> b) -> (a -> c)
f ◦ g = \x -> f (g x)

-----------------------------------------------------------------------------------
all :: (a -> Bool) -> [a] -> Bool
all p = and ◦ map p
--all p xs = and (map p xs) -- note: todo: is the same as the above


all' :: (a -> Bool) -> [a] -> Bool
all' f []                 = True
all' f (x:xs) | f x       = all' f xs
              | otherwise = False


all'' p xs = foldl (&&) True (map p xs)
all''' p = foldr (&&) True . map p
-----------------------------------------------------------------------------------
any   :: (a -> Bool) -> [a] -> Bool
any p = or ◦ map p

any' p xs = length (filter p xs) > 0

any'' p = not . null . dropWhile (not . p)

any3 p xs = not (all (\x -> not (p x)) xs )

any4 p xs = foldr (\x acc -> (p x) || acc) False xs

-----------------------------------------------------------------------------------
takeWhile       :: (a -> Bool) -> [a] -> [a]
takeWhile _ []  = [] -- since predicate is not used, say _
takeWhile p (x:xs)
    | p x       = x : takeWhile p xs
    | otherwise = []

-- note: is not the correct definition
takeWhile' p = foldl (\acc x -> if p x then x:acc else acc) []
-----------------------------------------------------------------------------------
dropWhile       :: (a -> Bool) -> [a] -> [a]
dropWhile _ []  = []
dropWhile p (x:xs)
    | p x       = dropWhile p xs
    | otherwise = x:xs

-----------------------------------------------------------------------------------
main = do
    print $ all even [1..10]
    print $ all even [2,4,6,8]
    print $ all odd [1,3,5,7]
    print $ all'' even [1..10]
    print $ all''' even [2,4,6]
    putStrLn "" ------------------------
    print $ any even [1..10]
    print $ any odd [2,4,6,8]
    putStrLn "--------------------------"
    putStrLn "any3: even: "
    print $ any3 even [1..10]
    print $ any3 even [2,4,6]
    print $ any3 even [1,3,5]
    putStrLn "" ------------------------
    putStrLn "--------------------------"
    putStrLn "any4: even: "
    print $ any4 even [1..10]
    print $ any4 even [2,4,6]
    print $ any4 even [1,3,5]
    putStrLn "" ------------------------
    print $ takeWhile even [2,4,6,7,8,9,3,1]
    print $ takeWhile isAlpha "abc def"
    putStrLn "--------------------------"
    putStrLn "takeWhile' : " -- note: is not the correct definition
    print $ takeWhile' even [2,4,6,7,8,9,3,1]
    print $ takeWhile' isAlpha "abc def"
    putStrLn "" ------------------------
    print $ dropWhile even [2,4,6,7,2,9]
    print $ dropWhile isSpace "     abc"