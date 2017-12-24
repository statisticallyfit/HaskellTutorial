import Prelude hiding (sum, odd)
import Data.Char
import Data.Foldable hiding (sum)

--7.2 Processing Lists -------------------------------------------------------------
-- map applies a function to every element of a list

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]


map''          :: (a -> b) -> [a] -> [b]
map'' f []       = []
map'' f (x:xs) = f x : map'' f xs


-- filter selects elements from a list that satisfy a given predicate
filter'     :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filter''          :: (a -> Bool) -> [a] -> [a]
filter'' p []     = []
filter'' p (x:xs) | p x       = x : filter p xs
                  | otherwise = filter p xs


sumSqrEven :: [Int] -> Int
sumSqrEven ns = sum (map (^2) (filter even ns))




-- 7.3 foldr function --------------------------------------------------------------

-- foldr encapusulates this pattern:
-- f []     = v
-- f (x:xs) = x (OP) f xs

--sum'     = foldr (+) 0 = x + sum xs
--product' = foldr (*) 0
--or'      = foldr (||) False
--and'     = foldr (&&) True
-- reverse = foldr (\x xs -> xs ++ [x]) []
-- (++ ys) = foldr (:) ys
-- help: todo: why gives error?


foldr'            :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []     = v
foldr' f v (x:xs) = f x (foldr f v xs)


length' xs = foldr (\_ n -> 1 + n) 0 xs


snoc x xs = xs ++ [x] -- snoc is cons backward
reverse' []     = []
reverse' (x:xs) = foldr snoc [] (x:xs)
--snoc x (reverse xs)


--(++ ys) = foldr (:) ys
-- help: todo: where does the argument go?





-- 7.4. foldl function -------------------------------------------------------------

-- this is the foldl pattern: uses an operator assumed to be left-associative.
sum = sum' 0
      where
        sum' v []     = v
        sum' v (x:xs) = sum' (v+x) xs
-- the f (v (OP) x) xs has 'v' as accumulator value.

--sum     = foldl (+) 0
--product = foldl (*) 1
--or      = foldl (||) False
--and     = foldl (&&) True
-- length = foldl (\n _ -> n + 1) 0 -- help: todo: what is n and _ mean?
-- reverse = foldl (\xs x -> x:xs) []
-- (xs ++) = foldl (\ys y ++ [y]) xs

-- for reverse:
-- note: replace each (:) by \x xs -> xs ++ [x]  and [] by []


foldl''            :: (a -> b -> a) -> a -> [b] -> a
foldl'' f v []     = v
foldl'' f v (x:xs) = foldl'' f (f v x) xs





-- 7.5 the composition operator ----------------------------------------------------
(◦) :: (b -> c) -> (a -> b) -> (a -> c)
f ◦ g = \x -> f (g x)

{- note: This operator could also be deﬁned by (f ◦ g) x = f (gx).
However, we prefer the above deﬁnition in which the x argument is shunted to
the body of the deﬁnition using a lambda expression, because it makes explicit
the idea that composition returns a function as its result.
-}

-- help: todo: why do these declarations work but the above sum, or, and, don't?
odd = not ◦ even
twice f = f ◦ f
sumSqrEven' = sum ◦ map (^2) ◦ filter even


-- composition of a list of functions
-- f ◦ id = f for any function f
compose :: [a -> a] -> (a -> a)
compose = foldr (◦) id


main = do ----------------------------------- map
    print $ map (+1) [1,3,5,7]
    print $ map isDigit ['a', '1', 'b', '2']
    print $ map reverse ["abc", "def", "ghi"]
    print $ map (map (+1)) [[1,2,3], [4,5]] -- is [map (+1) [1,2,3], map (+1) [4,5]]
    ------------------------------------------- filter
    print $ filter even [1..10]
    print $ filter (>5) [1..10]
    print $ filter (/= ' ') "abc def ghi"
    print $ sumSqrEven [1..10]
    -------------------------------------------- other higher order functions
    print $ all even [2,4,6,8]
    print $ any odd [2,4,6,8]
    print $ takeWhile isLower "abc def"
    print $ dropWhile isLower "abc def"
    --------------------------------------------- foldr
    print $ reverse' [1,2,3,4,5]
    --------------------------------------------- foldl
    print $ sum [1,2,3]