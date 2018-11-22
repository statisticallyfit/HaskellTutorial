
-- original
isSorted          :: [Integer] -> Bool
isSorted []       = False
isSorted [x]      = True
isSorted (x:y:xs) = (x <= y) && (isSorted (y:xs))


-- exericse 17 --------------------------------------------------------------------------
-- Part 1 - descending
isSortedDescending          :: [Integer] -> Bool
isSortedDescending []       = False
isSortedDescending [x]      = True
isSortedDescending (x:y:xs) = (x >= y) && (isSortedDescending (y:xs))


-- Part 2 - remove duplicates
isSortedNoDuplicates :: [Integer] -> Bool
isSortedNoDuplicates [] = False
isSortedNoDuplicates [x] = True
isSortedNoDuplicates (x:y:xs) = (x < y) && (isSortedNoDuplicates (y:xs))



main = do
    print $ isSortedDescending [10,4,3,2,1] -- true
    print $ isSortedNoDuplicates [1,2,2,3,4,4,5,1,2] --false
    print $ isSortedNoDuplicates [1,1,2,2,3,4,5,5,5] --false
    print $ isSortedNoDuplicates [3,1,4] -- false
    print $ isSortedNoDuplicates [1,2,3,4] -- true
