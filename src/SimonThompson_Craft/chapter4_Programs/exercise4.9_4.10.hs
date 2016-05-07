-- aid from: https://github.com/buntine/Haskell--Craft-of-FP/blob/master/Chapter4Exercises.lhs#L101
-- module  Statistics.Distribution.Binomial where

func :: Int -> Int
func 0 = 87
func 1 = 16
func 2 = 22
func 3 = 765
func 4 = 0
func 5 = 987
func _ = 0

pickMax :: Int -> Int
pickMax 0 = func 0
pickMax n = max (func n) (pickMax (n - 1))


anyZero :: Int -> Bool
anyZero 0 = func 0 == 0
anyZero n = func n == 0 || anyZero (n-1)


-- help: how to do this with f = binomial function?
--data BinomialDistribution = binomial 100 0.4

main = do
    print $ pickMax 10
    print $ anyZero 10; print $ anyZero 0; print $ anyZero 3
--    print (binomial 100 0.4)
    --print $ probability (BD 100 0.4)