
-- function 1
list = [2, 3 , 5, 7, 11]
total = sum (map(3*) list)


-- function 2
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial(n - 1)


-- function 3
hailstone :: Integer -> Integer
hailstone n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise      = 3 * n + 1


-- function 4
foo :: Integer -> Integer
foo 0 = 16
foo 1
    | "Haskell" > "C++"  = 3
    | otherwise = 4
foo n
    | n < 0           = 0
    | n `mod` 17 == 2 = -43
    | otherwise       = n + 3



-- function 5 - abstracting out evenness test in hailstone()
isEven :: Integer -> Bool
isEven n
    | n `mod` 2 == 0 = True
    | otherwise     = False




-- job 6

-- you can define a pair
pair :: (Int, Char)
pair = (3, 'x')




-- function 7
f x y z = x + y + z


-- this is how to make an empty list
-- emptyList = []
-- these are singly-linked lists, not arrays
--a = 1 : []
--c = [2, 3, 4] == 2 : 3 : 4 : []




-- function 8
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)





-- Functions on Lists

-- Compute the length of a list of Integers
intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (_ : xs) = 1 + intListLength xs




--- Sum every two elements (nested patterns)
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []
sumEveryTwo (x:[])     = []  -- ignore element if one element left
sumEveryTwo (x:y:zs) = (x + y) : sumEveryTwo zs



-- Combining functions
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1



main = do
    --putStrLn("hailstone 3: " , hailstone 3)
    --print ["foo: " ++ foo(-3), foo 0, foo 1, foo 36, foo 38]
    --print [ isEven 2, isEven 5, isEven -3, isEven -4]
    --print (f 3 17 8)
    --print(hailstoneSeq 23)
    --print (intListLength [1,2,3,4,5])
    --print (sumEveryTwo [1, 2, 3, 4, 5, 6, 7, 8, 9])
    print (hailstoneLen 10)






{-
* the = sign means definition, not assignment so x = 4 means x cannot be changed
So this is why y = y+1 is assumed recursive definition (going on forever)
-}