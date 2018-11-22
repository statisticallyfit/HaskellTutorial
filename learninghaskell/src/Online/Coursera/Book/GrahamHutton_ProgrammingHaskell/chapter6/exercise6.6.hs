import Prelude hiding (sum, take, last)
import Data.Foldable hiding (sum)


sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

-- help: todo: how does this work without giving the xs?
sum' :: Num a => [a] -> a
sum' = foldr (+) 0


take :: Int -> [a] -> [a]
take 0 _      = []
take n []     = []
take n (x:xs) = x : take (n-1) xs


last        :: [a] -> a
last [x]    = x
last (_:xs) = last xs


main = do
    print $ sum [1,3,5,6,9,3]
    print $ sum' [1,3,5,6,9,3]
    print $ take 3 [1,2,3,4,5]
    print $ last [5,3,2,41, 3]