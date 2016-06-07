import Test.QuickCheck



-- exercise 8 --------------------------------------------------------------------------
elemNum         :: Integer -> [Integer] -> Integer
elemNum x []    = 0
elemNum x (y:ys)
    | x == y    = 1 + elemNum x ys
    | otherwise = elemNum x ys


elemNum' :: Integer -> [Integer] -> Integer
elemNum' x ys = sum [1 | y <- ys, x == y]


-- exercise 9 --------------------------------------------------------------------------
unique :: [Integer] -> [Integer]
unique es = [e | e <- es, elemNum e es == 1]

-- had help here. answer
unique' :: [Integer] -> [Integer]
unique' []                  = []
unique' (a:as)
    | elemNum a as == 0 = a : unique' as
    | otherwise         = unique' (remove a as)



-- note these remove all duplicates of b in as.
remove      :: Integer -> [Integer] -> [Integer]
remove b as = [a | a <- as, a /= b]


remove'         :: Integer -> [Integer] -> [Integer]
remove' _ []    = []
remove' b (a:as)
    | a == b    = remove' b as
    | otherwise = a : remove' b as



-- HELP understand this better.
propElemNumUnique :: Integer -> [Integer] -> Bool
propElemNumUnique x xs =
    (elemNum x (unique xs)) `elem` [ 0 , 1 ]


main = do
    print $ elemNum 2 [1,2,2,5,3,4,2,6]
    print $ elemNum 2 [1,2,2,5,3,4,2]
    print $ elemNum 5 [1,2,2,5,3,4,2,6]

    print $ elemNum' 2 [1,2,2,5,3,4,2,6]
    print $ elemNum' 2 [1,2,2,5,3,4,2]
    print $ elemNum' 5 [1,2,2,5,3,4,2,6]
    putStrLn "" ---------------------------
    print $ unique [4,2,1,3,2,3]
    print $ unique' [4,2,1,3,2,3]
    putStrLn "" ---------------------------
    print $ remove 1 [3,3,2,2,1,1,1,1,4,5,1,6]
    print $ remove' 1 [3,3,2,2,1,1,1,1,4,5,1,6]
    putStrLn "" ---------------------------
    quickCheck propElemNumUnique