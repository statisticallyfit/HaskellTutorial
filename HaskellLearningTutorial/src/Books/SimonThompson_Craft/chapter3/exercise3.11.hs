
{-Give definitions of the functions
min :: Int -> Int -> Int
minThree
:: Int -> Int -> Int -> Int
which calculate the minimurn of two and three integers, respectively
-}

import Prelude hiding (min)

min :: Int -> Int -> Int
min x y
    | x <= y    = x
    | otherwise = y


minThree :: Int -> Int -> Int -> Int
minThree x y z
    | x <= y && x <= z = x
    | y <= z           = y
    | otherwise        = z




main = do
    print (min (-100) 10)
    print (minThree  1 2 3)