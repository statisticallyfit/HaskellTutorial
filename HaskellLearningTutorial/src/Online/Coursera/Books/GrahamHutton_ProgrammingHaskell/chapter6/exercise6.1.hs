import Prelude hiding ((^), exp )

(^)   :: Int -> Int -> Int
a ^ 0 = 1
a ^ b = a * (a ^ (b-1))

exp m 1 = m
exp m n = m * (exp m (n-1))

main = do
    print ( 2^3)
    print $ exp 2 3; print $ exp 4 3

