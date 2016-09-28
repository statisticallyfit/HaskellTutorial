


--- testing that result tree is sorted.
testInsertOrder :: Int -> Tree Int -> Bool
testInsertOrder n tree = (isSorted $ collapse $ insert n tree)


--- testing that if element is not already inside tree, then length should be greater
-- by 1, else they should be same size.
testInsertSize :: Int -> Tree Int -> Bool
testInsertSize n oldTree
    | occurs n oldTree = (size newTree) ==  (size oldTree)
    | otherwise        = (size newTree) == (size oldTree + 1)
    where newTree = insert n oldTree




--- generates a BST (in order)
--- source: http://www.seas.upenn.edu/~cis552/12fa/lectures/stub/BST.html
instance (Ord a, Bounded a, Random a, Num a, Arbitrary a) => Arbitrary (BinaryTree a)  where
   arbitrary = gen 0 100 where
      gen :: (Ord a, Num a, Random a) => a -> a -> Gen (BinaryTree a)
      gen min max | (max - min) <= 3 = return Leaf
      gen min max = do
        elt <- choose (min, max)
        frequency [ (1, return Leaf),
                    (6, liftM3 Node (gen min (elt - 1))
                                    (return elt)
                                    (gen (elt + 1) max)) ]

instance Eq a => EqProp (Tree a) where (=-=) = eq

--- testing function
isSorted xs = (sort xs) == xs



---------------------------------------------------------------------

--- NOTE alhtough this one doesn't generate elements in order,
--- use asinspiration for making above definition faster:
--- NOTE faster tree declaration
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbTree
arbTree 0 = return Nil --liftM NilT arbitrary
arbTree n = frequency [(1, return Nil),
                       (4, liftM3 Node arbitrary (arbTree (n `div` 2))
                                                 (arbTree (n `div` 2)) )]
