
-- assume m <= n
rangeProduct :: Int -> Int -> Int
rangeProduct m n
    | m < n     = m * rangeProduct (m+1) n
    | m == n    = n
    | otherwise = 0


factorial :: Int -> Int
factorial n = rangeProduct 1 n


main = do
    print $ rangeProduct 3 7
    print $ rangeProduct 1 2
    print $ rangeProduct 0 1
    print $ rangeProduct 0 0
    print $ rangeProduct 1 1
    print $ factorial 4; print $ factorial 5; print $ factorial 6