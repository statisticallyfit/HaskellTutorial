

findRoot :: Int -> Int -> Int
findRoot n sqt
    | sqt^2 > n = sqt - 1
    | otherwise = findRoot n (sqt + 1)


integerSquareRoot :: Int -> Int
integerSquareRoot n = findRoot n 1


main = do
    print $ integerSquareRoot 20
    print $ integerSquareRoot 10
    print $ integerSquareRoot 16
    print $ integerSquareRoot 25
    print $ integerSquareRoot 23
    print $ integerSquareRoot 101
