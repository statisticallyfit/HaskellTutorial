

data GTree a = Leaf a | Gnode [GTree a] deriving (Eq, Show)



t0 = Leaf 2
t1 = Gnode [Leaf 1, Leaf 3]
t2 = Gnode [Leaf 1]
t12345 = Gnode [Gnode [Leaf 1, Leaf 2], t10, Gnode [Leaf 4, Leaf 5]]
t1245 =  Gnode [Gnode [Leaf 1, Leaf 2], Gnode [Leaf 4, Leaf 5]]
t3 = Gnode [Gnode [Leaf 7, Gnode [Leaf 3, Gnode [Leaf 111, Gnode [Gnode [Leaf 8]]]]]]
t4 = Gnode [Gnode [Gnode [Leaf 3]]]
t5 = Gnode [Gnode [Gnode []]]
t6 = Gnode [Gnode [Leaf 10]]
-- NOTE as soon as pattern is Leaf, Node, Leaf then it throws exception.
--  pattern Node, Leaf, Node also results in error.
t7 = Gnode [Leaf 1, Gnode [Gnode [Gnode [Gnode [Leaf 3, Gnode [Gnode [Gnode [Leaf 10]], Leaf 23]]], Leaf 41]],
    Leaf 44, Gnode [Leaf 1]]
t8 = Gnode [Gnode [Leaf 7, Gnode [Leaf 3], Leaf 10, Leaf 5, Gnode [Leaf 1]]]
t9 = Gnode [Gnode [Leaf 1, Leaf 2, Gnode [Leaf 8, Gnode [Leaf 9], Leaf 7]]]
t10 = Gnode [Gnode [Leaf 7, Gnode [Leaf 3]]]
t11 = Gnode [Leaf 1, Gnode [Leaf 2], Leaf 3]


countLeaves :: Eq a => GTree a -> Integer
countLeaves (Leaf _) = 1
countLeaves (Gnode ts) = count [Gnode ts]
{-countLeaves (Gnode (t:ts))
    | ts == [] = countL [t]
    | otherwise = countL [t] + countL ts-}

count :: [GTree a] -> Integer
count [Leaf _] = 1
count [Gnode []] = 0
count [Gnode (t:[])] = count [t]
count [Gnode (t:ts)] = count [t] + count ts

{-
depth :: GTree a -> Integer
depth (Leaf _) = 1 -- note leaf will count as extra layer compared to no leaf at edge
depth (Gnode (t:ts)) = max (depth t) (depth ts)


sumGTree :: GTree Int -> Int
sumGTree (Leaf n) = n
sumGTree (Gnode (t:ts)) = sumGTree t + sumGTree ts


occurs :: Eq a => a -> GTree a -> Bool
occurs elem (Leaf n) = elem == n
occurs elem (Gnode (t:ts)) = occurs elem t || occurs elem ts


mapGTree :: (a -> b) -> GTree a -> GTree b
mapGTree f (Leaf n) = Leaf (f n)
mapGTree f (Gnode (t:ts)) = Gnode (mapGTree f t) (mapGTree f ts)


flattenGTree :: GTree a -> [a]
flattenGTree (Leaf n) = [n]
flattenGTree (Gnode (t:ts)) = flattenGTree t ++ flattenGTree ts
-}
