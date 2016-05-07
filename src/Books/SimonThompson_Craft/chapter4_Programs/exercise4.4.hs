
count      :: Int -> [Int] -> Int
count x xs = length [x' | x' <- xs, x == x']


threeEqual :: Int -> Int -> Int -> Int -> Bool
threeEqual a b c d
    | count min [a,b,c,d] == 3 = True
    | count max [a,b,c,d] == 3 = True
    | otherwise                = False
    where min = minimum [a,b,c,d]
          max = maximum [a,b,c,d]


fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual a b c d = a == b && c == d && b == c


fourDiff :: Int -> Int -> Int -> Int -> Bool
fourDiff a b c d = a /= b && a /= c && a /= d &&
                   b /= c && b /= d &&
                   c /= d


howManyOfFourEqual :: Int -> Int -> Int -> Int -> Int
howManyOfFourEqual a b c d
    | fourEqual a b c d  = 4
    | threeEqual a b c d = 3
    | fourDiff a b c d   = 0
    | otherwise          = 2


main = do
    print $ fourEqual 1 2 3 4; print $ fourEqual 2 2 3 3
    print $ fourEqual 1 1 1 3; print $ fourEqual 10 10 10 10
    putStrLn ""
    ----------------------------------------------------------
    print $ fourDiff 1 2 3 4; print $ fourDiff 2 2 3 3
    print $ fourDiff 1 1 1 3; print $ fourDiff 10 10 10 10
    putStrLn ""
    ----------------------------------------------------------
    print $ howManyOfFourEqual 1 1 1 2
    print $ howManyOfFourEqual 2 2 2 1
    print $ howManyOfFourEqual 1 2 1 2
    print $ howManyOfFourEqual 4 4 3 3
    print $ howManyOfFourEqual 1 2 3 4
    print $ howManyOfFourEqual 8 8 8 8
    print $ howManyOfFourEqual 0 0 1 1
    print $ howManyOfFourEqual 0 0 0 0