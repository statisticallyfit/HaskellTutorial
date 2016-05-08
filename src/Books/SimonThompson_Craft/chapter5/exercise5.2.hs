
-- from exercise 4.2 --------------------------------------------------------------
weakAscendingOrder :: Int -> Int -> Int -> Bool
weakAscendingOrder m n p
    | m <= n && p >= n = True
    | m >= n && p <= n = True
    | otherwise        = False


-- true only if n is between m and p
between :: Int -> Int -> Int -> Bool
between m n p = weakAscendingOrder m n p || weakAscendingOrder p n m


middleNumber :: Int -> Int -> Int -> Int
middleNumber a b c
    | between b a c = a
    | between a b c = b
    | otherwise     = c

-- from exercise 4.1 --------------------------------------------------------------
maxThree :: Int -> Int -> Int -> Int
maxThree x y z = (x `max` y) `max` z
-----------------------------------------------------------------------------------
minThree :: Int -> Int -> Int -> Int
minThree x y z = (x `min` y) `min` z
-----------------------------------------------------------------------------------


-- Exercise 5.2 (this one)
orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (a,b,c) = (theMin, theMid, theMax)
                      where theMin = minThree a b c
                            theMid = middleNumber a b c
                            theMax = maxThree a b c



main = do
    print $ orderTriple (1,1,1)
    putStrLn ""
    print $ orderTriple (4,4,5)
    print $ orderTriple (4,5,4)
    print $ orderTriple (5,4,4)
    print $ orderTriple (4,4,3)
    print $ orderTriple (4,3,4)
    print $ orderTriple (3,4,4)
    putStrLn ""
    print $ orderTriple (1,2,3)
    print $ orderTriple (1,3,2)
    print $ orderTriple (2,1,3)
    print $ orderTriple (2,3,1)
    print $ orderTriple (3,1,2)
    print $ orderTriple (3,2,1)
    putStrLn ""