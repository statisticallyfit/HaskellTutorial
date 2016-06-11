import Data.Char

-- MAPPING
-- taking second element of each pair in list of pairs
-- formatting each (n,p) pair in a list

 -- FILTERING
 -- getting digits out of an address

-- FOLDING (combining items)
-- folding a function (+) over a list with seed (0)
-- foling (++) over a list of lists to concatenate
-- folding (&&) over a list of booleans to take their conjunction.
-- folding max over a list to give maximum.


-- BREAKING UP LISTS
-- getWord, dropSpace...



-- COMBINATIONS OF ALL THESE



 -- 10.2 HIGHER ORDER FUNCTIONS --------------------------------------------------------

-- 1 MAP
-- map :: (a -> b) -> [a] -> [b]
doubleAll        :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x:xs) = 2*x : doubleAll xs

doubleAll' xs = [2*x | x <- xs]

doubleAll'' xs = map (2*) xs


convertChrs :: [Char] -> [Int]
convertChrs xs = map fromEnum xs


-- 2 FILTER
-- filter :: (a -> Bool) -> [a] -> [a]
digits xs = filter isDigit xs
evens xs = filter even xs



-- ZIP and MAP ==> ZIPWITH
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]






-- 10.3. FOLDING -----------------------------------------------------------------------

{-
NOTE foldr1
foldr1          :: (a -> a -> a) -> [a] -> a
foldr1 f [x]    = x
foldr1 f (x:xs) = f x (foldr1 f xs)

EXAMPLE
(+) :: (a -> a -> a)
list :: [a]
result :: a

in

foldr1 (+1) [3,98,1] = 102

EXAMPLEs
foldr1 (||) (False, True, False] = True
foldr1 (++) ["Freak ", "Out", "!"] = "Freak Out!"
foldr1 min [6] = 6
foldr1 (*) [1..6] = 720




NOTE foldr --- only different thing is the seed value (a) before [a]

foldr            :: (a -> a -> a) -> a -> [a] -> a
foldr f s []     = s
foldr f s (x:xs) = f x (foldr f s xs)

EXAMPLES
concat :: [[a]] -> [a]
concat xss = foldr (++) [] xs

and :: [Bool] -> Bool
and bs = foldr (&&) True bs



NOTE Equals
foldr1 f xs = foldr f (last xs) (init xs)
-- equals (because seed last xs) will be fed in at the end.

-}