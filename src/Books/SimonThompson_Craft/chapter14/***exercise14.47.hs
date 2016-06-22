import Test.QuickCheck

data GTree a = Leaf a | Gnode [GTree a] deriving (Eq, Show)

mapGTree :: (a -> b) -> GTree a -> GTree b
mapGTree f (Leaf n) = Leaf (f n)
mapGTree f (Gnode ts) = Gnode (map (mapGTree f) ts)


-----------------------------------------------------
collapseGTree :: GTree a -> [a]
collapseGTree (Leaf n) = [n]
collapseGTree (Gnode ts) = concat $ map collapseGTree ts



propMapCollapse :: Eq b => (a -> b) -> GTree a -> Bool
propMapCollapse f tree = map f (collapseGTree tree) == collapseGTree (mapGTree f tree)


-- HELP why won't this compile?
--eval = quickCheckWith stdArgs {maxSuccess = 1000} propMapCollapse

{-
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Proposition: map f (collapse tr) = collapse (mapTree f tr)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

data GTree a = Leaf a | Gnode [GTree a]


^^^^^^^^^^^^^
1. BASE CASE:
^^^^^^^^^^^^^

LEFT
map f (collapse (Leaf a))
= map f [n]
= [f n]

RIGHT
collapse (mapTree f (Leaf a))
= collapse (Leaf (f n))
= [f n]



^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
2. ASSUME INDUCTION HYPOTHESIS
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

map f (collapse t1) = collapse (mapTree f t1)
map f (collapse t2) = collapse (mapTree f t2)



^^^^^^^^^^^^^
3. INDUCTION
^^^^^^^^^^^^^

LEFT
map f (collapse (Gnode ts))
= map f (concat $ map collapseTree ts)

-- HELP

RIGHT
collapse (mapTree f (Gnode ts))
= collapse (Gnode (map (mapTree f) ts))
-- HELP

-}