module BinomialTheorem where



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
infixl 7 .*
(.*) :: Num a => a -> [a] -> [a]
c .* [] = []
c .* (f:fs) = c*f : c .* fs

z :: Num a => [a]
z = [0, 1]

--- note: the lists carry polynomial coefficients, not the function values. 
instance (Num a, Ord a) => Num [a] where
    fromInteger c = [fromInteger c]

    negate [] = []
    negate (f:fs) = (negate f) : (negate fs)

    signum [] = []
    signum gs | signum (last gs) < (fromInteger 0) = negate z
              | otherwise = z

    abs [] = []
    abs gs | signum gs == z = gs
           | otherwise = negate gs

    -- noteoverloading of (+): in (f+g) we have (+) of numbers but for (fs + gs) ...
    fs + [] = fs
    [] + gs = gs
    (f:fs) + (g:gs) = f + g : fs + gs

    fs * [] = []
    [] * gs = []
    (f:fs) * (g:gs) = f*g : (f .* gs + fs * (g:gs))



--- note computes the difference list (1-z) * f(z)
-- example (z+1)^4 = [1,4,6,4,1] (pascal's triangle)
-- help understand why 1-z == [1,-1]
-- answer because z = [0,1] means z(x) = x + 0 = x
delta :: (Num a, Ord a) => [a] -> [a]
delta = ((1 - z) * )



--- note composition of polynomials is:  f(g(z)) = f0 + g(z) * f.tail(g(z))
-- note we can use this to pick arbitrary layer in Pascal's triangle
-- example: comp1 (z^3) (z+1) ==> [1,3,3,1]
-- note can also use this to generate Pascal's triangle to arbitrary depth:
-- example: comp1 [1,1,1,1,1,1] [[0], [1,1]]
-- which means f(z) 1+z+z^2+z^3+z^4+z^5+z^6 composed with
-- g(z) = (y+1)z + 0
comp1 :: (Num a, Ord a) => [a] -> [a] -> [a]
comp1 _ [] = error ".."
comp1 [] _ = []
comp1 (f:fs) gs = [f] + (gs * comp1 fs gs)
----------------- f0  + gs * f.tail (g(z))


comp :: (Num a, Ord a) => [a] -> [a] -> [a]
comp _ [] = error ".."
comp [] _ = []
comp (f:fs) (0:gs) = f : gs * (comp fs (0:gs))
comp (f:fs) (g:gs) = ([f] + [g] * (comp fs (g:gs))) + (0 : gs * (comp fs (g:gs)))


deriv :: (Num a, Ord a) => [a] -> [a]
deriv [] = []
deriv (f:fs) = deriv' fs 1
    where deriv' [] _ = []
          deriv' (g:gs) n = n*g : deriv' gs (n+1)