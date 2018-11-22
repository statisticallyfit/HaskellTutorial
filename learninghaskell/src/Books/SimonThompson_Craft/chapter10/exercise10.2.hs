

length' :: [a] -> Int
length' xs = sum $ map one xs


-- had help here
one :: a -> Int
one _ = 1