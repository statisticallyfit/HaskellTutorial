type Pair = (Integer, Integer)



iSortPairs        :: [Pair] -> [Pair]
iSortPairs []     = []
iSortPairs [p]     = [p]
iSortPairs (p:ps) = insertPair p (iSortPairs ps)



-- precondition: the list y:ys is sorted ascendingly
insertPair :: Pair -> [Pair] -> [Pair]
insertPair p []     = [p]
insertPair p1 (p2 : ps)
    | p2 `greaterOrEqual` p1 = p1 : p2 : ps
    | otherwise              = p2 : insertPair p1 ps
    where greaterOrEqual (x1,y1) (x2,y2) =
            if x1 > x2 then True
            else (
                if x1 == x2 && y1 >= y2
                then True
                else False
            )


main = do
    print $ insertPair (5,4) [(1,1),(3,2),(10,2)]
    print $ iSortPairs [(5,4),(3,2), (3,2), (1,1), (3,2), (10,2), (1,1),(1,1),(1,1)]