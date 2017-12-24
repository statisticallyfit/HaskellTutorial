import Data.List


factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0 ]

-- a positive integer is perfect if it equals the sum of its factors,
-- excluding the number itself
perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], isPerfect x]
             where isPerfect num = sum (init (factors num)) == num


main = do
    print $ perfects 500