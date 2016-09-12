module BinomialTheorem where


choose :: Int -> Int -> Int
choose n k = (product [(n-k+1)..n]) `div` (product [1..k])

choose' :: Int -> Int -> Int
choose' n 0 = 1
choose' n k
    | n < k = 0
    | n == k = 1
    | otherwise = choose' (n-1) (k-1) + choose' (n-1) k 