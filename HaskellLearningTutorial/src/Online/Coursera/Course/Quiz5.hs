module Online.Coursera.Course.Quiz5 where

-- exercise 11
xs = 1 : [x + 1 | x <- xs]


-- exercise 12
riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x,y] | (x,y) <- zip xs ys]



-- exercise 13
divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0


divisors :: Int -> [Int]
divisors n = [x | x <- [1 .. n], n `divides` x]


main = do
    --print xs -- recursive infinite list
    print $ riffle [1,2,3] [4,5,6]
    print $ divides 15 2; print $ divides 15 3
    print $ divisors 15; print $ divisors 14
