-- help: todo how to import my file threeDiff,Equal...
--module exercise5.1 where
--import FileToImport (howManyEqual)
maxThree :: Int -> Int -> Int -> Int
maxThree x y z = (x `max` y) `max` z
------------------------------------------------------------------------------
-- from  exercise 4.3
threeEqual :: Int -> Int -> Int -> Bool
threeEqual a b c = (a == b) && (b == c)


threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = (a /= b) && (b /= c)


howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
    | threeEqual a b c     = 3
    | threeDifferent a b c = 0
    | otherwise            = 2

------------------------------------------------------------------------------
-- from exercise 4.4
count      :: Int -> [Int] -> Int
count x xs = length [x' | x' <- xs, x == x']

threeEqualOfFour :: Int -> Int -> Int -> Int -> Bool
threeEqualOfFour a b c d
    | count min [a,b,c,d] == 3 = True
    | count max [a,b,c,d] == 3 = True
    | otherwise                = False
    where min = minimum [a,b,c,d]
          max = maximum [a,b,c,d]


fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual a b c d = a == b && c == d && b == c

------------------------------------------------------------------------------
-- first
maxOccurs      :: Int -> Int -> (Int, Int)
maxOccurs a b
    | a > b     = (a, 1)
    | a < b     = (b, 1)
    | otherwise = (a, 2)


-- second exercise part
occursOfNum                     :: Int -> Int -> Int -> Int -> Int
occursOfNum num a b c
    | fourEqual num a b c        = 3
    | threeEqualOfFour num a b c = 2
    | otherwise                  = 1


occursOfMax :: Int -> Int -> Int -> Int -> Int
occursOfMax max a b c = occursOfNum max a b c


maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs a b c = (theMaxNum, occursOfMax theMaxNum a b c)
                        where theMaxNum = maxThree a b c


main = do
    print $ maxOccurs 3 4
    print $ maxOccurs 4 1
    print $ maxOccurs 5 5
    putStrLn ""
    ---------------------------
    print $ maxThreeOccurs 4 4 5
    print $ maxThreeOccurs 4 5 4
    print $ maxThreeOccurs 5 4 4 --
    print $ maxThreeOccurs 4 4 2
    print $ maxThreeOccurs 4 2 4
    print $ maxThreeOccurs 2 4 4 --
    print $ maxThreeOccurs 17 1 3
    print $ maxThreeOccurs 2 8 9
    print $ maxThreeOccurs 2 9 8
    print $ maxThreeOccurs 3 3 3
