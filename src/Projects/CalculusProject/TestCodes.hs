module TestCodes where

import Codes
import Types
import ArbitraryInst

import Data.List

import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, frequency)

------------------------------------------------ TESTS---------------------------------------------
--- Hel.per methods
get :: Code -> [Const]
get (Poly ps) = ps


---------------------------------------------------------------------------------------------------


-- test: addPoly produces polynomial with correct Const and power (index of Const in list)
testAddPolyCorrect :: Code -> Code -> Bool
testAddPolyCorrect p@(Poly ps) q@(Poly qs) = addPoly p q == result
    where
        (Poly ps', Poly qs') = fillZeroes p q
        result = Poly $ map sum (transpose [ps', qs'])
testAddPolyCorrect _ _ = True








main = do
    quickCheck testAddPolyCorrect