import Data.Char -- for isLower

-- 1. GENERATORS in list comprehensions -------------------------------------

-- Mathematics comprehension notation is used with generators in
-- notation called "list comprehension"
-- The expression x <- [1..5] is the generator.
g1 :: [Integer]
g1 = [x^2 | x <- [1..5] ]

-- List comprehensions are like nested for loops
g2 :: [(Integer, Integer)]
g2 = [ (x, y) | x <- [1,2,3], y <- [4,5] ]

g3 :: [(Integer, Integer)]
g3 = [ (x, y) | y <- [4,5], x <- [1,2,3] ]

-- later generators can depend upon values of variables from
-- earlier generators
g4 :: [(Integer, Integer)]
g4 = [ (x,y) | x <- [1..3], y <- [x..3] ]


flatten :: [[a]] -> [a]
flatten xss = [x | xs <- xss, x <- xs]


firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]


length' :: [a] -> Int
length' xs = sum [1 | _ <- xs] -- the _ is just a counter






-- 2. GUARDS in list comprehensions -------------------------------------------

-- guards filter values made by earlier generators
evens :: [Integer]
evens = [x | x <- [1..10], even x]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0 ]

-- not all factors of a non-prime number are made due to lazy evaluation.
-- as soon as the list of factors is not equal to [1,n] prime returns False.
isPrime :: Int -> Bool
isPrime n = factors n == [1, n]


primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], isPrime x ]

-- returns the list of all values associated with the key given to find from
-- the given table
find :: Eq a => a -> [(a, b)] -> [b] -- type 'a' must be instance of Eq class
find key table = [value | (key', value) <- table,  key == key']
----------- return value | such that         it matches my given key





-- 3. Using ZIP function ------------------------------------------------------
ex :: [(Char, Int)]
ex = zip ['a', 'b', 'c'] [1,2,3,4]


pairs    :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- lazily evaluated, so that as soon as one pair does not fit the x <= y,
-- the function returns false.
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs] --first create pairs, then check x <= y
-- help: todo: why do you need 'and' at the beginning? And with what?


positions      :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                 where n = length xs - 1
-- procedure:
-- 1. zip xs with their indexes
-- 2. assign the pos-xvalue pair into a tuple
-- 3. if the xs in the zipped tuple match the given x, then return its index.





-- 4. STRING COMPREHENSIONS --------------------------------------------------

-- list comprehensions can be used to define functions on strings
lowers    :: String -> Int
lowers xs = length [x | x <- xs, isLower x]
-- note: given that the x value is in lower case, return it

count      :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']
-- note: put each char one at a time in x', and if x' is the same as the
-- note: given value to look for, then return that x' and take the length
-- note: of the created list.



main = do ------------------------------------------------------ generators
    print (g1); print (g2); print (g3); print (g4)
    print (flatten [[1,2,3], [3, 4], [1], []])
    print (firsts [('a', 5), ('b', 1), ('c', 3)])
    print (length' [1,2,3,4,5,6,7,8,9,10])
    putStrLn ""
    ------------------------------------------------------------ guards
    print (evens)
    print (factors 50); print $ factors 15; print $ factors 89
    print $ isPrime 10; print $ isPrime 7
    print $ primes 89; print $ primes 10; print $ primes 40
    print $ find 'b' [ ('a',1), ('b',2), ('c',3), ('b',4) ]
    putStrLn ""
    ------------------------------------------------------------ zip
    print ex
    print $ pairs [1,2,3,4]; print $ pairs [1,2,3,4,5]
    print $ sorted [1,2,3,4,5]; print $ sorted [9,8,3,1,10]
    print $ positions 8 [8,3,2,4,8,-3,1,8,0]
    putStrLn ""
    ------------------------------------------------------------ string comp
    print $ lowers "Haskell"; print $ lowers "HI THERE"
    print $ count 's' "Mississippi"