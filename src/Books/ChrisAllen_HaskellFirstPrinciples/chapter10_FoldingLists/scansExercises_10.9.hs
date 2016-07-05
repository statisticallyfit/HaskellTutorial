
{-
NOTE
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f q ls =
    q : (case ls of
            []     -> []
            (x:xs) -> scanl f (f q x) xs)

example evaluation:

 1 :: (1: scanl (+) 1 fibs)
:1 :: (1: scanl (+) 1 fibs)
:1+(1 :: (1: scanl (+) 1 (1: scanl (+) 1 fibs))
:2+(1 :: (1: scanl (+) 1 (1: scanl (+) 1 fibs)))
:3+(2 :: (1: scanl (+) 1 (1: scanl (+) 1 (1: scanl (+) 1 fibs)))
:5+(3 :: (1: scanl (+) 1 (1: scanl (+) 1 (1: scanl (+) 1 fibs)))
:8+(5 :: (1: scanl (+) 1 (1: scanl (+) 1 (1: scanl (+) 1 (1: scanl (+) 1 fibs))))
-}

fibs = 1 : scanl (+) 1 fibs
fibN n = fibs !! n



--- 1
firstTwentyFibs = take 20 fibs


--- 2
lessThanHundredFibs = takeWhile (< 100) fibs


--- 3
-- todo help understand better: is this factorial because next element is the accumulated
-- product so we multiply that by next val?
factorial = scanl (*) 1 [1..]