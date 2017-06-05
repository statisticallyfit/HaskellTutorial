{-# LANGUAGE FlexibleContexts #-}
module Polynomial where


import Types

import Data.List
import Data.Char
import Data.Maybe
import Data.Ratio hiding (show)

import Test.QuickCheck
import Test.QuickCheck.Arbitrary


------- UTIL --------------------------------------------------------------------------------------

makeFraction :: Int -> Int -> Fraction
makeFraction num denom = Rate $ num % denom

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




---------------------------------------------------------------------------------------------------
--- POLYNOMIAL CODE



-- Adds the polynomials in the Codes.
addPoly :: Code -> Code -> Code
addPoly (Poly p) (Poly q) = Poly (zipWith (+) p' q')
    where
    (Poly p', Poly q') = fillZeroes (Poly p) (Poly q)






