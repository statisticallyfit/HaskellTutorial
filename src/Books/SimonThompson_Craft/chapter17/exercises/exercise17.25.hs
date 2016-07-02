import Test.QuickCheck

-- NOTE I think I used memoization!
runningSums :: [Int] -> [Int]
runningSums numbers = runner numbers [0]  -- starting with 0 as first element.
    where runner [] cumSums = cumSums
          runner (n:ns) cumSums = runner ns (cumSums ++ [last cumSums + n])


testRunningSumsLength :: [Int] -> Bool
testRunningSumsLength xs = (length $ runningSums xs) == (length xs + 1)

testRunningSumsLastElementIsTotal :: [Int] -> Bool
testRunningSumsLastElementIsTotal xs = last (runningSums xs) == (sum xs)



-- help todo understand better how this works. page 472
-- help how do the sums get to the front of the list? in front of zipwith? 
listSums :: [Integer] -> [Integer]
listSums iList = out
    where out = 0 : zipWith (+) iList out