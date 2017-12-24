module Lab3 where

-----------------------------------------------------------------------------------------------------------------------------
-- LIST COMPREHENSIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0 - 2
-- ===================================

evens :: [Integer] -> [Integer]
evens xs = [x | x <- xs, even x ]

-- ===================================
-- Ex. 3 - 4
-- ===================================

-- complete the following line with the correct type signature for this function
-- squares :: ...
squares   :: Integer -> [Integer]
squares 0 = []
squares n = [x^2 | x <- [1..n]]

-- help: todo: what does this error mean?
{-}`Integer' is applied to too many type arguments
    In the type signature for `squares':
    squares :: Integer a => a -> [a]
-}


sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)



-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function
-- squares' :: ...
squares' m n = [x^2 | x <- [(n+1) .. (m+n)]]

sumSquares' :: Integer -> Integer
sumSquares' x = sum . uncurry squares' $ (x, x)

-- ===================================
-- Ex. 8
-- ===================================

coords :: Integer -> Integer -> [(Integer,Integer)]
coords m n = [(x,y) | x <- [0..m], y <- [0..n]]



main = do
    print $ evens [2,5,6,13,32]
    print $ coords 1 1
    print $ coords 1 2
    print $ foldr (-) 0 . map (uncurry (*)) $ coords 5 7