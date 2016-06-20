

data GTree a = Leaf a | Gnode [GTree a] deriving (Eq, Show)



t0 = Leaf 2
t1 = Gnode [Leaf 1, Leaf 3]
t2 = Gnode [Leaf 1]
t12345 = Gnode [Gnode [Leaf 1, Leaf 2], t10, Gnode [Leaf 4, Leaf 5]]
t1245 =  Gnode [Gnode [Leaf 1, Leaf 2], Gnode [Leaf 4, Leaf 5]]
t3 = Gnode [Gnode [Leaf 7, Gnode [Leaf 3, Gnode [Leaf 111, Gnode [Gnode [Leaf 8]]]]]]
t4 = Gnode [Gnode [Gnode [Leaf 3]]]
t5 = Gnode [Leaf 4, Gnode [Gnode [Leaf 5], Leaf 432], Leaf 2]
t6 = Gnode [Gnode [Leaf 10]]
-- TODO HELP NOTE find out how to deal with pattern Gnode []
-- For now this pattern is not allowed!
t7 = Gnode [Leaf 1, Gnode [Gnode [Gnode [Gnode [Leaf 3, Gnode [Gnode [Gnode [Leaf 10]], Leaf 23]]], Leaf 41]],
    Leaf 44, Gnode [Leaf 1]]
t8 = Gnode [Gnode [Leaf 7, Gnode [Leaf 3], Leaf 10, Leaf 5, Gnode [Leaf 1]]]
t9 = Gnode [Gnode [Leaf 1, Leaf 2, Gnode [Leaf 8, Gnode [Leaf 9], Leaf 7]]]
t10 = Gnode [Gnode [Leaf 7, Gnode [Leaf 3]]]
t11 = Gnode [Leaf 1, Gnode [Leaf 2], Leaf 3]




countLeaves :: GTree a -> Integer
countLeaves (Leaf _) = 1
countLeaves (Gnode ts) = sum $ map countLeaves ts

-----------------------------------------------------
depth :: GTree a -> Integer
depth (Leaf _) = 1
depth (Gnode ts) = 1 + (maximum $ map depth ts) -- note need these parens else error!!

-----------------------------------------------------
sumGTree :: GTree Integer -> Integer
sumGTree (Leaf n) = n
sumGTree (Gnode ts) = sum $ map sumGTree ts

-----------------------------------------------------
occurs :: Eq a => a -> GTree a -> Bool
occurs elem (Leaf n) = elem == n
occurs elem (Gnode ts) = or $ map (occurs elem) ts

-----------------------------------------------------

mapGTree :: (a -> b) -> GTree a -> GTree b
mapGTree f (Leaf n) = Leaf (f n)
mapGTree f (Gnode ts) = Gnode (map (mapGTree f) ts)


-----------------------------------------------------
flattenGTree :: GTree a -> [a]
flattenGTree (Leaf n) = [n]
flattenGTree (Gnode ts) = concat $ map flattenGTree ts
-- HELP understand evaluation.

-----------------------------------------------------



-- HELP exercise 14.22 - how to make completely empty tree?



-- NOTE sample of how I did it before using map (before got blocked defining
-- mapGTree
{-
sumG :: [GTree Integer] -> Integer
sumG [Leaf n] = n
sumG [Gnode (t:[])] = sumG [t]
sumG [Gnode (t:ts)] = sumG [t] + sumG ts
sumG (t:[]) = sumG [t]
sumG (t:ts) = sumG [t] + sumG ts
-}