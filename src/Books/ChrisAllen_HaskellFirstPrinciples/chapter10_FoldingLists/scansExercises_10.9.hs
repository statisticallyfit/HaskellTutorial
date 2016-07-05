
{-
NOTE
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f q ls =
    q : (case ls of
            []     -> []
            (x:xs) -> scanl f (f q x) xs)

example evaluation:

fibs = 1 : scanl (+) 1 fibs
= 1 : case fibs of
        [] -> []
        (1: [1]) -> scanl (+) ((+) 1 1) [1]
               -> scanl (+) 2 [1]

= 2 : case fibs of
        [] -> []
        (
-}

fibs = 1 : scanl (+) 1 fibs
fibN n = fibs !! n



--- 1
firstTwentyFibs = take 20 fibs


--- 2
lessThanHundredFibs = takeWhile (< 100) fibs


--- 3
factorial =