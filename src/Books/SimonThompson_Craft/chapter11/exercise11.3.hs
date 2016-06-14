

composeList :: [(a -> a)] -> a -> a
composeList fs = foldl (.) id fs

composeList' :: [(a -> a)] -> a -> a
composeList' fs = foldr (.) id fs


composeList'' :: [(a -> a)] -> (a -> a) -- note is same as above notation
composeList'' [] = id -- attaches function id to the end of the list.
composeList'' (f:fs) = f . composeList'' fs


f1 :: Int -> Int
f1 x = x + 1


main = do
    print $ g 2
        where g = (composeList [(+ 1), (+ 1), (+ 1), (+ 1), (+ 1)])


{-
NOTE

zipWith ($) [sum,product] [[1,2],[3,4]]
[3,12]
*Main> zipWith ($) [sum,product] [[3,12],[1,2]]
[15,2]

-}