import Prelude hiding (zip3)


zip3          :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 _ _ []   = []
zip3 _ [] _   = []
zip3 [] _ _   = []
--zip3 _ [] []  = []
--zip3 [] _ []  = []
--zip3 [] [] _  = []
--zip3 [] [] [] = []
zip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3 xs ys zs



zip3'' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3'' _ _ [] = []
zip3'' _ [] _ = []
zip3'' [] _ _ = []
zip3'' xs ys zs = fmap toTriple $ zip xs (zip ys zs)
                  where toTriple (as, (bs, cs)) = (as, bs, cs)





zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' [] [] [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3' xs ys zs
zip3' _ _ _ = [] -- help does this replace all the blank spot matching from zip3 above?

main = do
    print $ zip3 [1,2,3] ['a', 'b', 'c'] ['#', '@', '!']
    print $ zip3 [1,2,3,4,5] ['a', 'b', 'c', 'd'] ['#', '@', '!']
    putStrLn "" ---------------------------------------------------
    print $ zip3' [1,2,3] ['a', 'b', 'c'] ['#', '@', '!']
    print $ zip3' [1,2,3,4,5] ['a', 'b', 'c', 'd'] ['#', '@', '!']
    putStrLn "" ---------------------------------------------------
    print $ zip3'' [1,2,3] ['a', 'b', 'c'] ['#', '@', '!']
    print $ zip3'' [1,2,3,4,5] ['a', 'b', 'c', 'd'] ['#', '@', '!']