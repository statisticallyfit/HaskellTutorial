

total :: (Integer -> Integer) -> (Integer -> Integer)
total f = \n -> sum $ map (\x -> f x) [0..n]