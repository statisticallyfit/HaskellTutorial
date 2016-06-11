

squares :: [Integer] -> [Integer]
squares ns = map (^2) ns

sumSquares :: [Integer] -> Integer
sumSquares ns = sum $ squares ns

allGreaterZero :: [Integer] -> Bool
allGreaterZero ns = ns == (filter (> 0) ns)