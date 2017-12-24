import Prelude hiding (and, concat, replicate, (!!), elem)


and        :: [Bool] -> Bool
and []     = True
and (b:bs) = b && and (bs)




concat          :: [[a]] -> [a]
concat []     = []
--concat [[x]]    = [x]
concat (xs:xss) = xs ++ concat xss


replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n v = v : replicate (n-1) v


(!!)        :: [a] -> Int -> a
(x:_) !! 1 = x
(_:xs) !! n = xs !! (n-1)



elem :: Eq a => a -> [a] -> Bool
elem _ []     = False
elem e (x:xs) = e == x || (elem e xs)


elem'     :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) | e == x    = True
               | otherwise = elem e xs


main = do
    print $ and [True, True, False]
    print $ and [True]; print $ and [False]
    print $ and [False, False, False]; print $ and [True, True, True]
    putStrLn ""
    print $ concat [[1,2,3], [3,4], [5]]
    putStrLn ""
    print $ replicate 3 'a'; print $ replicate 10 True
    putStrLn ""
    print $ [1,2,3,4,5] !! 3
    putStrLn ""
    print $ elem 3 [1,2,3,4,5]
    print $ elem' 9 [1,2,3,4,5]
    print $ elem' 3 [1,2,3,4,5]
