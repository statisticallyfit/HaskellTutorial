
import Prelude
import Data.List

averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral (a + b + c) / 3
-- help: todo: I thought fromIntegral takes a tuple?
-- help: todo: why does it work with Int args, since fromInt must use
-- help: todo: Integer args.


-- the $ function application just changes the function application
-- from left associative to right-associative
howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c = length $ filter (> avg) theThree
    where avg      = averageThree a b c
          theThree = [fromIntegral a, fromIntegral b, fromIntegral c]--fromInteger ([a, b, c])


-- another way to do it
howManyAboveAverage' :: Int -> Int -> Int -> Int
howManyAboveAverage' a b c = length [i | i <- [a, b, c], fromIntegral i > avg]
    where avg = averageThree a b c



main = do
    print (averageThree 80 99 95)
    print (averageThree 100 78 83)
    print (howManyAboveAverage 100 78 83)
    print (howManyAboveAverage' 100 78 83)
