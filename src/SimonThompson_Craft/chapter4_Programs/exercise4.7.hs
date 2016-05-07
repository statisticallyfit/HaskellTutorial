-- assume: domain is natural numbers: 0,1,2,3,4 ...
mult :: Int -> Int -> Int
mult n m
    | m == 0 || n == 0 = 0
    | m == 1           = n
    | m > 0            = mult (m-1) n + n



main = do
    print $ mult 7 4