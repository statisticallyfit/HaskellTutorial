

removeOne       :: Eq a => a -> [a] -> [a]
removeOne x []  = []
removeOne x (y:ys)
    | x == y    = ys
    | otherwise = y : removeOne x ys


-- note: checks if first list is a subset of second list
-- How: checks if first element of first list is present in the second list.
-- If it is, return True, then continue with the rest of the first list and with
-- the second list that has removed that value from it.
-- When no more first list, then return True since of course empty list is a
-- subset of anything.
-- When first list but empty second list, then false, since cannot have a full
-- list as subset of empty second list.
{-
NOTE:
isChoice [1,2,3] [1,2,3, 4]
True
isChoice [1,2,3,5] [1,2,3, 4]
False
-}
isChoice           :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice (x:xs) [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (removeOne x ys)

