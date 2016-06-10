import Test.QuickCheck


-- 9.2 TESTING AND PROOF -----------------------------------------------------------------

-- note fails for when two elemenst are equal
mysteryMax :: Int -> Int -> Int -> Int
mysteryMax x y z
    | x > y && x > z = x
    | y > x && y > z = y
    | otherwise      = z


propMystery :: Int -> Int -> Int -> Bool
propMystery x y z = mysteryMax x y z == (x `max` y) `max` z


{-
NOTE random testing not reliable:

quickCheck propMystery
+++ OK, passed 100 tests.
*Main> quickCheck propMystery
*** Failed! Falsifiable (after 16 tests and 1 shrink):
8
8
0
*Main> quickCheck propMystery
*** Failed! Falsifiable (after 2 tests):
1
1
0


-}








-- 8.4 JUSTIFICATION -----------------------------------------------------------------

doubleAll :: [Integer] -> [Integer]
doubleAll [] = []
doubleAll (z:zs) = 2*z : doubleAll zs


-- test the property
-- sum after doubling = 2 * sum xs

propSumDoubleAll :: [Integer] -> Bool
propSumDoubleAll xs = sum (doubleAll xs) == (2 * sum xs)