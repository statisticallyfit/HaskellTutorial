module TestPolynomial where

import Polynomial
import Types
import ArbitraryDeclarations

import Data.List

import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, frequency)

------------------------------------------------ TESTS---------------------------------------------
--- Hel.per methods
get :: Code -> [Coeff]
get (Poly p) = p


---------------------------------------------------------------------------------------------------


-- test: addPoly produces polynomial with correct coefficient and power (index of coeff in list)
testAddPolyCorrect :: Code -> Code -> Bool
testAddPolyCorrect p q = addPoly p q == result
    where
        (Poly p', Poly q') = fillZeroes p q
        result = Poly $ map sum (transpose [p', q'])








main = do
    quickCheck testAddPolyCorrect