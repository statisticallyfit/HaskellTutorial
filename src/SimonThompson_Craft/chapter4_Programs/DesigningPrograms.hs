
toPower :: Int -> Int -> Int
toPower base exp
    | exp == 0 = 1
    | exp > 0  = base * toPower base (exp - 1)


fac :: Int -> Int
fac n
    | n == 0 = 1
    | n > 0  = n * fac (n-1)

--------------------------------------------------------------------
sumFacs :: Int -> Int
sumFacs n
    | n == 0 = 1
    | n > 0  = sumFacs (n-1) + fac n


sumFunc :: (Int -> Int) -> Int -> Int
sumFunc f n
    | n == 0 = f 0
    | n > 0  = sumFunc f (n-1) + f n


regions :: Int -> Int
regions n
    | n == 0 = 1
    | n > 0  = regions (n-1) + n

--------------------------------------------------------------------

fib :: Int -> Int
fib n
    | n == 0 = 0
    | n == 1 = 1
    | n > 1  = fib (n - 2) + fib (n - 1)


divide :: Int -> Int -> Int
divide m n
    | m < n = 0
    | otherwise = 1 + divide (m - n) n

remainder :: Int -> Int -> Int
remainder m n
    | m < n     = m
    | otherwise = remainder (m - n) n



main = do
    print $ toPower 2 2; print $ toPower 2 3; print $ toPower 2 4
    print $ toPower 5 3; print $ toPower 1 100
    print $ sumFacs 4; print $ sumFacs 5
    print $ sumFunc fac 4
    --------------------------------------------------------------
    putStr "Remainder 37 9 = "; print $ remainder 37 9
    putStr "Dividing  37 9 = "; print $ divide 37 9
