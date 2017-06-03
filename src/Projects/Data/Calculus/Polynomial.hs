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
addPoly (Poly ps) (Poly qs) = Poly (zipWith (+) ps' qs')
    where
    (ps', qs') = fillZeroes (Poly ps) (Poly qs)







------------------------------------------------ TESTS------------------------------------------------------------
--- A h.elper method for extracting the array out of the polynomial
get :: Code -> [Fraction]
get (Poly p) = p


-- TODO: replace the other elongate with this new one
-- Adds zeroes to the end of one of the lists inside the Code so they are the same length.
fillZeroes :: Code -> Code -> (Code, Code)
fillZeroes c1@(Poly p1) c2@(Poly p2)
    | len1 > len2 = (c1, c2 ++ zeroes)
    | len1 < len2 = (c1 ++ zeroes, c2)
    | otherwise = (c1, c2)
    where
        len1 = length p1
        len2 = length p2
        zero = makeFraction 0 1
        zeroes = replicate (abs (len1 - len2)) zero



-- test: addPoly produces polynomial with correct coefficient and power (index of coeff in list)
testAddPolyCorrect :: Code -> Code -> Bool
testAddPolyCorrect c1 c2 = addPoly c1 c2 == result
    where
        (Poly p1, Poly p2) = fillZeroes c1 c2
        result = Poly $ map sum (transpose [p1, p2])