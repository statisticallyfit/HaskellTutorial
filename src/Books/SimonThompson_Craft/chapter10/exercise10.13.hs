
-- note 1 to n
sumSquares' :: Integer -> Integer
sumSquares' n = sum $ map (^2) [1..n]


sumSquares :: Integer -> Integer
sumSquares n = foldr (+) 0 (map (^2) [1..n])