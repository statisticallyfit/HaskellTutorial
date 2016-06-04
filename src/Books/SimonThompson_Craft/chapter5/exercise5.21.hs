
matches      :: Integer -> [Integer] -> [Integer]
matches m xs = [p | p <- xs, m == p]


isElem :: Integer -> [Integer] -> Bool
isElem elem xs = length (matches elem xs) /= 0