

maxOccurs      :: Integer -> Integer -> (Integer, Integer)
maxOccurs a b
    | a > b     = (a, 1)
    | a < b     = (b, 1)
    | otherwise = (a, 2)


main = do
    print $ maxOccurs 3 4
    print $ maxOccurs 4 1
    print $ maxOccurs 5 5