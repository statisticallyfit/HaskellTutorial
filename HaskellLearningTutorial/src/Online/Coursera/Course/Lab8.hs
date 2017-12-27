module Online.Coursera.Course.Lab8 where

import Data.Char (ord, chr)
------------------------------------------------------------------------------------------------------------------------------
-- RECURSIVE FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------

import Data.Char

-- ===================================
-- Ex. 0
-- ===================================

triangle :: Integer -> Integer
triangle 0 = 0
triangle n = n + triangle (n-1)

-- ===================================
-- Ex. 1
-- ===================================

count :: Eq a => a -> [a] -> Int
count a [] = 0
count a (x:xs)
    | a == x    = 1 + count a xs
    | otherwise = count a xs

xs = [1,2,35,2,3,4,8,2,9,0,5,2,8,4,9,1,9,7,3,9,2,0,5,2,7,6,92,8,3,6,1,9,2,4,8,7,1,2,8,0,4,5,2,3,6,2,3,9,8,4,7,1,4,0,1,8,4,1,2,4,56,7,2,98,3,5,28,4,0,12,4,6,8,1,9,4,8,62,3,71,0,3,8,10,2,4,7,12,9,0,3,47,1,0,23,4,8,1,20,5,7,29,3,5,68,23,5,6,3,4,98,1,0,2,3,8,1]

ys = map (\x -> ((x + 1) * 3) ^ 3 - 7) xs
{-

p = ["Three Types for the Lisp-kings under the parentheses," ++
    "Seven for the Web-lords in their halls of XML," ++
    "Nine for C Developers doomed to segfault," ++
    "One for the Dark Lord on his dark throne" ++
    "In the Land of Haskell where the Monads lie." ++
    "One Type to rule them all, One Type to find them," ++
    "One Type to bring them all and in the Lambda >>= them" ++
    "In the Land of Haskell where the Monads lie."
    ]
-}

poem = [ "Three Types for the Lisp-kings under the parentheses,"
       , "Seven for the Web-lords in their halls of XML,"
       , "Nine for C Developers doomed to segfault,"
       , "One for the Dark Lord on his dark throne"
       , "In the Land of Haskell where the Monads lie."
       , "One Type to rule them all, One Type to find them,"
       , "One Type to bring them all and in the Lambda >>= them"
       , "In the Land of Haskell where the Monads lie."
       ]


encrypt :: [Char] -> [Int]
encrypt = \word -> map (ord . \letter -> chr (ord letter + 4)) word

testCount = count 101 (poem >>= encrypt)

-- ===================================
-- Ex. 2
-- ===================================

-- Example:
-- euclid (5,7) = euclid (5,2) = euclid (3,2) = euclid (1,2) = euclid (1,1) = 1
euclid :: (Int,  Int) -> Int
euclid (x, y)
    | x == y = x
    | otherwise = euclid ((x `max` y) - (x `min` y), (x `min` y))

-- ===================================
-- Ex. 3
-- ===================================

-- Example:
--   funkyMap (+10) (+100) [1, 2, 3, 4, 5]
-- = [(+10) 1, (+100) 2, (+10) 3, (+100) 4, (+10) 5]
funkyMap              :: (a -> b) -> (a -> b) -> [a] -> [b]
funkyMap f g []       = [] -- if even number of elements
funkyMap f g [a]      = [f a] -- if odd number of elements
funkyMap f g (a:b:cs) = f a : g b : funkyMap f g cs

funkyMap' f g = zipWith ($) (cycle [f,g])
-- note:
-- = funkyMap' (+1) (+100) [1,2,3,4,5]
-- = [2, 102, 4, 104, 6]


f1 = (\letter -> if letter == 'e' then 1 else 0)
g1 = ord

testFunkyMap = sum $ funkyMap f1 g1 (poem >>= id)












-- HELP
-- exericse 12:
-- what is the type of foldr id: why is it that way?

 -- HELP mull over rest of exercises


-- Exercise 15
h g f = (f . g) $ f
-- HELP type of h?


-- Exercise 16
fix = h fix -- HELP why does it have type (a -> a) -> a ?





-- Exercise 18
f2 = \f2 n -> if (n == 0) then 1 else n * f2(n-1)
-- HElp mull over


-- Exercise 19
k = fix $ f2
