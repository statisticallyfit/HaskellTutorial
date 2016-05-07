

threeEqual :: Int -> Int -> Int -> Bool
threeEqual a b c = (a == b) && (b == c)


threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = (a /= b) && (b /= c)


howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
    | threeEqual a b c     = 3
    | threeDifferent a b c = 0
    | otherwise            = 2


main = do
    print (howManyEqual 1 2 3)
    print (howManyEqual 1 1 2)
    print (howManyEqual 3 3 3)
