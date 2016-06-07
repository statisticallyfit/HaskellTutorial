
{-

isSorted        :: [Integer] -> Bool
isSorted []     = True
isSorted (x:y:xs)
    | x > y     = False
    | otherwise = and $ isSorted (y:xs)
-}

isSorted          :: [Integer] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:xs) = (x <= y) && (isSorted (y:xs))