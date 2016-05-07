

maxOccurs      :: Integer -> Integer -> (Integer, Integer)
maxOccurs a b
    | a > b     = (a, 1)
    | a < b     = (b, 1)
    | otherwise = (a, 2)


--maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
--maxThreeOccurs a b c
--    |


main = do
    print $ maxOccurs 3 4
    print $ maxOccurs 4 1
    print $ maxOccurs 5 5