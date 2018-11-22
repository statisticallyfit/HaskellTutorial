qSort :: [Integer] -> [Integer]
qSort [] = []
qSort (x:xs) = qSort [y | y <- xs, y <= x] ++ [x] ++ qSort [y | y <- xs, y > x]


qSortDescending :: [Integer] -> [Integer]
qSortDescending [] = []
qSortDescending (x:xs) = qSortDescending [y | y <- xs, y > x]
                            ++ [x] ++
                         qSortDescending [y | y <- xs, y <= x]



qSortRemoveDuplicates :: [Integer] -> [Integer]
qSortRemoveDuplicates [] = []
qSortRemoveDuplicates (x:xs) = qSortRemoveDuplicates [y | y <- xs, y > x]
                               ++ [x] ++
                               qSortRemoveDuplicates [y | y <- xs, y < x]



main = do
    print $ qSortDescending [4,2,1,8,4,5,6,7]
    print $ qSortDescending [2]
    putStrLn "" ------------------------------------------------------
    print $ qSortRemoveDuplicates  [4,2,1,1,1,1,8,4,5,5,5,3,3,3,6,7]
    print $ qSortRemoveDuplicates  [4,2,1,8,4,5,6,7]
    print $ qSortRemoveDuplicates [2,2,2,2,2]
    print $ qSortRemoveDuplicates [2]

