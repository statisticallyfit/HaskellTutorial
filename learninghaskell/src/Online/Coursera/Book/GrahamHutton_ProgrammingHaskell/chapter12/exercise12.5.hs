fibList :: [Integer]
fibList = 0 : 1 : [x + y | (x,y) <- zip fibList (tail fibList)]


fib     :: Int -> Integer
fib nth = head (drop (nth - 1) fibList)
{-
fib nth = case take 1 (drop (nth - 1) fibList) of
            [x] -> x
-}

fib' nth = fibList !! (nth - 1)

firstFibGreaterThanOneThousand = head (dropWhile (<= 10000) fibList)