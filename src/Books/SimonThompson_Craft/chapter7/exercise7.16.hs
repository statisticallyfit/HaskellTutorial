import Data.List hiding (insert )


-- Part 1: sort list in descending order ------------------------------------------------

-- note sorting descendingly
iSortDescending        :: [Integer] -> [Integer]
iSortDescending []     = []
iSortDescending (x:xs) = insertDescending x (iSortDescending xs)

-- precondition: the list y:ys is sorted descendingly
insertDescending :: Integer -> [Integer] -> [Integer]
insertDescending x []     = [x]
insertDescending x (y:ys)
    | x >= y    = x : y : ys
    | otherwise = y : insertDescending x ys




-- Part 2: duplicates are removed while sorting ascendingly -----------------------------
-- postcondition:: result list is ascendingly sorted with duplicates removed.
iSortNoDup        :: [Integer] -> [Integer]
--iSortNoDup []     = []
iSortNoDup [x]    = [x]
iSortNoDup (x:xs) = insertNoDup x (iSortNoDup xs)


-- precondition - assume duplicates are in y:ys list.
-- note first remove all duplicates then insert.
insertNoDup     :: Integer -> [Integer] -> [Integer]
insertNoDup x ys
    | elem x ys = ys
    | otherwise = insert x ys


-- precondition: the list y:ys is sorted ascendingly. Duplicates are allowed.
-- postcondition: the result list is sorted ascendingly and duplicates are removed.
insert           :: Integer -> [Integer] -> [Integer]
insert x []      = [x]
insert x (y:ys)
    | y >= x     = x : y : ys
    | otherwise  = y : insert x ys
