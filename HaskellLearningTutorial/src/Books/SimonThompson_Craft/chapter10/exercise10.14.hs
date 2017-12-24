

sumSquaresPos' :: [Integer] -> Integer
sumSquaresPos' xs = sum $ map (^2) (filter (> 0) xs)


sumSquaresPos :: [Integer] -> Integer
sumSquaresPos xs = foldr (+) 0 (map (^2) (filter (> 0) xs))