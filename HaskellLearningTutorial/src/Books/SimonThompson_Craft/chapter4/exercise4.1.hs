
maxThree :: Int -> Int -> Int -> Int
maxThree x y z = (x `max` y) `max` z


maxFour :: Int -> Int -> Int -> Int -> Int
maxFour a b c d = max d (maxThree a b c)


maxFour' :: Int -> Int -> Int -> Int -> Int
maxFour' a b c d = ((a `max` b) `max` c) `max` d

maxFour'' :: Int -> Int -> Int -> Int -> Int
maxFour'' a b c d
    | a >= maxThree b c d = a
    | otherwise           = maxThree b c d


main = do
    print (maxThree 1 2 3)
    print (maxFour (-1) 10 5 3)
    print (maxFour' (-1) 10 5 19)
    print (maxFour'' (-1) 10 5 19)