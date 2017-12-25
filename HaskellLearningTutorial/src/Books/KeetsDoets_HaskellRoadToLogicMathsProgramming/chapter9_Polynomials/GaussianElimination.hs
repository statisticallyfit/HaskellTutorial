module GaussianElimination where

import Data.Ratio
import DifferenceAnalysis

{-
todo once we have automated gauss elim:
[13,21,35,55,81,113,151]

Example: given list of function values, find its form using structure:
function values: [-7, -2, 15, 50, 109, 198, 323]

a                 = 7
a +  b +  c +   d = 2
a + 2b + 4c +  8d = 15
a + 3b + 9c + 27d = 50
-}


type Row = [Integer]
type Matrix = [Row]


rows, cols :: Matrix -> Int
rows m = length m
cols m | m == [] = 0
       | otherwise = length (head m)



--- produces appropriate matrix for a list generated by a polynomial
genMatrix :: [Integer] -> Matrix
genMatrix xs = zipWith (++) (genM d) [[x] | x <- xs]
    where d = degree xs
          -- note first the m iterates then the x list iterates
          -- so: 0^0, 0^1, 0^2, 0^3, 1^0, 1^1, 1^2...
          genM n = [ [ (toInteger x^m) | m <- [0 ..n] ] | x <- [0..n]]




--- note used in gaussian elimination by using the first given row to adjust
-- the second given row
adjustWith :: Row -> Row -> Row
adjustWith (m:ms) (n:ns) = zipWith (-) (map (n*) ms) (map (m*) ns)


-- puts in echelon form
{-
1. if num rows or num cols == 0 then matrix is already in echelon form
2. if every row of matrix begsinw ith 0 then echelon or rs is 0 in front of
the echelon of the sub (inner right) matrix
3. if matrix has rows that don't start with 0, then take one row (say piv)
 and use it to eliminate leading coeffs from the other rows.
-}
echelon :: Matrix -> Matrix
echelon rs
    | null rs || null (head rs) = rs
    | null rs2                  = map (0:) (echelon (map tail rs))
    | otherwise                 = piv : map (0:) (echelon rs')
    where rs'            = map (adjustWith piv) (rs1 ++ rs3)
          (rs1, rs2)     = span leadZero rs
          leadZero (n:_) = n == 0
          (piv : rs3)    = rs2

--- help understand what this does
eliminate :: Rational -> Matrix -> Matrix
eliminate p rs = map (simplify c a) rs
    where
    c = numerator p
    a = denominator p
    simplify c a row = init (init row') ++ [a*d - b*c]
        where
        d = last row
        b = last (init row)
        row' = map (*a) row

--- help understand what this does
backsubst :: Matrix -> [Rational]
backsubst rs = backsubst' rs []
    where
    backsubst' [] ps = ps
    backsubst' rs ps = backsubst' rs' (p:ps)
        where
        a = (last rs) !! ((cols rs) - 2)
        c = (last rs) !! ((cols rs) - 1)
        p = c % a
        rs' = eliminate p (init rs)

--- note returns list in form where elements are rations in rising order of degree
-- so that n^3 + 3*n^2 + n - 7 means [-7 % 1, 1 % 1, 3 % 1, 1 % 1]
-- and     (1/4)n^4 + (1/2)n^3 + (1/4)n^2  is [0 % 1,0 % 1,1 % 4,1 % 2,1 % 4]
solveSeq :: [Integer] -> [Rational]
solveSeq = backsubst . echelon . genMatrix




--- feeding the polynomial from solveSeq a value (x) to get the f(x) result, so
-- in essence we are going back to step 1 where we just had a list of values.
--- testing map (toPoly (solveSeq ys)) ys == ys
toPoly :: Num a => [a] -> a -> a
toPoly [] x = 0
toPoly (a:as) x = a + (x * toPoly as x)


as = [-7, -2, 15, 50, 109, 198, 323]