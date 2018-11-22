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



{-
NOTE

1. base case

LEFT:
sum (doubleAll [])
= sum []
= 0

RIGHT:
2 * sum []
= 2 * 0
= 0



2. induction step

induction hypothesis: sum (doubleAll xs) == 2 * sum xs

LEFT:
sum (doubleAll (x:xs))
= sum (2*x : doubleAll xs)
= 2*x + sum (doubleAll xs)

RIGHT:
2 * sum (x:xs)
= 2 * (x + sum xs)
= 2*x + 2 * sum xs



LAST step to equate the two:

sum (doubleAll (x:xs))
= sum (2*x : doubleAll xs)
= 2*x + sum (doubleAll xs)
= 2*x + 2 * sum xs
-}





{-
NOTE how to prove recursive functions:

1. state as quickcheck property
2. state goal of induction and sub goals (base) (hyp) => (ind)
3. use arithmetic and functin definition to simplify sub goals.
4. label each step with its justification.
-}

-- EXAMPLE 1 ---> length and (++)

propLengthPlusPlus :: [a] -> [a] -> Bool
propLengthPlusPlus xs ys = length (xs ++ ys) == length xs + length ys

{-
Use the fact that (++) is defined as:
[]     ++ zs = zs
(w:ws) ++ zs = w : (ws ++ zs)


1. BASE CASE: ------------------------------------------------------------------------
length ([] ++ ys) = length [] + length ys


LEFT
length ([] ++ ys)
= length ys

RIGHT
length [] + length ys
= 0 + length ys
= length ys


2. INDUCTION HYPOTHESIS: -------------------------------------------------------------
length (xs ++ ys) = length xs + length ys

(assume)

3. INDUCTION STEP --------------------------------------------------------------------
length ((x:xs) ++ ys) = length (x:xs) + length ys

(prove)

LEFT
length ((x:xs) ++ ys)
= length (x:(xs ++ ys))   --- applied definition of (++)
= 1 + length (xs ++ ys)
= 1 + length xs + length ys


RIGHT
length (x:xs) + length ys
= 1 + length xs + length ys

-}



-- EXAMPLE 2 ----> reverse and ++
-- propReversePlusPlus       :: Eq a => [a] -> [a] -> Bool
-- note use non-generic type because otherwise all elements are the same.
propReversePlusPlus       :: [Integer] -> [Integer] -> Bool
propReversePlusPlus xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

{-

Use the fact that reverse is defined as:
reverse [] = []
reverse (z:zs) = reverse zs ++ [z]


1. BASE -----------------------------------------------------------------------------

reverse ([] ++ ys) = reverse ys ++ reverse []

LEFT
reverse ([] ++ ys)
= reverse ys

RIGHT
reverse ys ++ reverse []
= reverse ys ++ []  -- NOTE must prove this is an identity operation



2. HYP ------------------------------------------------------------------------------

reverse (xs ++ ys) = reverse ys ++ reverse xs

(assume)

3. IND ------------------------------------------------------------------------------
reverse ((x:xs) ++ ys) = reverse ys ++ reverse (x:xs)
(prove)


LEFT
reverse ((x:xs) ++ ys)
= reverse (x:(xs ++ ys))
= reverse (xs ++ ys) ++ [x]
= (reverse ys ++ reverse xs) ++ [x]

RIGHT
reverse ys ++ reverse (x:xs)
= reverse ys ++ (reverse xs ++ [x])       NOTE must prove (++) is associative
                                          NOTE xs ++ (ys ++ zs) = (xs ++ ys) ++ zs


-}