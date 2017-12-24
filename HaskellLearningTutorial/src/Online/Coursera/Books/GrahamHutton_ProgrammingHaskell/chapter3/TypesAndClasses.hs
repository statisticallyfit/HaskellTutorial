import Data.Char(isDigit)


-- takes a tuple and returns Int
add :: (Int, Int) -> Int
add (x,y) = x + y

zeroTo :: Int -> [Int]
zeroTo n = [0..n]



-- Currying
-- takes an int and returns function that takes and Int and returns Int
add' :: Int -> (Int -> Int)
add' x y = x + y

-- takes 3 Ints one at a time returning functions each time
mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z



-- Polymorphic types: take one or more type variables
-- length can calculate the length of lists with any type




-- Operators act within class constraints.
-- (+) :: Num a => a -> a -> a
-- means for any type 'a' that is an instance of the class Num, the
-- function (+) has type a -> a -> a
-- Paranthesizing an operator turns it into a curried function, so
-- (+) 1 2 is curried but
-- 1 + 2 is not


-- Overloaded type: a type that contains one or more class constraints.
-- example: (+), (-), (*), ...
-- Numbers themselves are overloaded also:
-- example: 3 :: Num a => a means for any type 'a', 3 has type 'a'.
-- todo: understand better.



-- Classes: all basic types Bool, Char, String ... are instances of
-- the Eq class provided that their element and component types are
-- instances of the class.
-- example: [1,2] == [1,2,3]


-- todo: why error using Fractional arguments?
division :: Float -> Float -> Float
division x y = x / y



-- type?
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs -- Eq a => [a] -> Bool


-- type ?
twice f x = f (f x) -- (a -> a) -> a -> a


main = print ""
--print (division 7 2)
--print (add(3, 4), add' 3 4, mult 5 6 7,  zeroTo(10))
--print (isDigit 'a') --todo: how to take multiple args? pg 21




--- QUIZ:
-- Exercise 12
--   Which of the following is not a valid list in Haskell:
--    ['1',['2','3']]   -- not valid
--    [(+),(−),(∗)]     -- is valid todo: why? is above not valid?
