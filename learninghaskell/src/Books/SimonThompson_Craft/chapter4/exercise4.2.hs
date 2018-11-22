
weakAscendingOrder :: Int -> Int -> Int -> Bool
weakAscendingOrder m n p
    | m <= n && p >= n = True
    | m >= n && p <= n = True
    | otherwise        = False


-- true only if n is between m and p
between :: Int -> Int -> Int -> Bool
between m n p = weakAscendingOrder m n p || weakAscendingOrder p n m


middleNumber :: Int -> Int -> Int -> Int
middleNumber a b c
    | between b a c = a
    | between a b c = b
    | otherwise     = c


main = do
    print (between 1 9 10)
    print (between (-2) 0 8)
    print (between 2 3 3)
    print (between 1 1 (-1))
    print (middleNumber 1 2 3)
    print (middleNumber 10 9 8)
    print (middleNumber 4 1 5)