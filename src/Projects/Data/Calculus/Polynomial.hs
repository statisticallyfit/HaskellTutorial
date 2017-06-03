{-# LANGUAGE FlexibleContexts #-}
module Polynomial where


import Types

import Data.List
import Data.Char
import Data.Maybe
import Data.Ratio hiding (show)



------- UTIL --------------------------------------------------------------------------------------

makeFraction :: Int -> Int -> Fraction
makeFraction num denom = Rate $ num % denom

---------------------------------------------------------------------------------------------------




-- Adds the polynomials in the Codes.
addPoly :: Code -> Code -> Code
addPoly (Poly p) (Poly q) = Poly (zipWith (+) p' q')
    where
    (Poly p', Poly q') = fillZeroes (Poly p) (Poly q)







------------------------------------------------ TESTS------------------------------------------------------------
--- A h.elper method for extracting the array out of the polynomial
get :: Code -> [Fraction]
get (Poly p) = p


-- TODO: replace the other elongate with this new one
-- Adds zeroes to the end of one of the lists inside the Code so they are the same length.
fillZeroes :: Code -> Code -> (Code, Code)
fillZeroes c@(Poly p) k@(Poly q)
    | len1 > len2 = (c, Poly (q ++ zeroes))
    | len1 < len2 = (Poly (p ++ zeroes), k)
    | otherwise = (c, k)
    where
        len1 = length p
        len2 = length q
        zero = makeFraction 0 1
        zeroes = replicate (abs (len1 - len2)) zero



-- test: addPoly produces polynomial with correct coefficient and power (index of coeff in list)
testAddPolyCorrect :: Code -> Code -> Bool
testAddPolyCorrect p q = addPoly p q == result
    where
        (Poly p', Poly q') = fillZeroes p q
        result = Poly $ map sum (transpose [p', q'])