import Test.QuickCheck
import Prelude hiding (maximum, minimum)
import qualified Prelude



iSort        :: [Integer] -> [Integer]
iSort []     = []
iSort (x:xs) = insert x (iSort xs)

insert :: Integer -> [Integer] -> [Integer]
insert x []     = [x]
insert x (y:ys)
    | y >= x    = x : y : ys
    | otherwise = y : insert x ys


-- exercise 12 --------------------------------------------------------------------------

maximum :: [Integer] -> Integer
maximum [] = 0 -- note so that last doesn't give an exception
maximum xs = last $ iSort xs

maximum'        :: [Integer] -> Integer
maximum' [x]    = x
maximum' (x:y:xs)
    | y > x     = maximum' (y:xs)
    | otherwise = maximum' (x:xs)



----------------------------------------------------
minimum :: [Integer] -> Integer
minimum [] = 0 -- note so that head doesn't give an exception
minimum xs = head $ iSort xs

minimum'        :: [Integer] -> Integer
minimum' [x]    = x
minimum' (x:y:xs)
    | y > x     = minimum' (x:xs)
    | otherwise = minimum' (y:xs)


-- HELP fix here since Prelude functions return exceptions and props can't be compared.
propMax xs = maximum xs == Prelude.maximum xs
propMin xs = minimum xs == Prelude.minimum xs
propMax' xs = maximum' xs == Prelude.maximum xs
propMin' xs = minimum' xs == Prelude.minimum xs
