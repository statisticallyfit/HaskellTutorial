module BinomialTheorem where

import Prelude hiding (negate, (+))
import qualified Prelude 



choose :: Int -> Int -> Int
choose n k = (product [(n-k+1)..n]) `div` (product [1..k])

choose' :: Int -> Int -> Int
choose' n 0 = 1
choose' n k
    | n < k = 0
    | n == k = 1
    | otherwise = choose' (n-1) (k-1) + choose' (n-1) k


--- note the more efficient law when 0 < k <= n
binom :: Int -> Int -> Int
binom n 0 = 1
binom n k | n < k = 0
          | otherwise = (n * binom (n-1) (k-1)) `div` k



--------------- Operations on Polynomials ------------------------------
-- negating function values
negate :: [Int] -> [Int]
negate [] = []
negate (f:fs) = (Prelude.negate f) : (negate fs)


-- just add coefficients of polynomials to add the polynomials
-- noteoverloading of (+): in (f+g) we have (+) of numbers but for (fs + gs)
--we have addition of polynomial coefficient sequences.
fs + [] = fs
[] + gs = gs
(f:fs) + (g:gs) = f Prelude.+ g : fs + gs

