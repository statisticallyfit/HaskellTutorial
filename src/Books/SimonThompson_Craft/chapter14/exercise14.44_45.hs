import Test.QuickCheck
import Control.Monad


data NTree = NilT | NodeT Integer NTree NTree deriving (Eq, Show)

tree1, tree2 :: NTree
tree1 = NodeT 10 NilT NilT
tree2 = NodeT 17 (NodeT 14 NilT NilT) (NodeT 20 NilT NilT)
tree3 = NodeT 3 (NodeT 4 NilT NilT) NilT

sumNTree :: NTree -> Integer
sumNTree NilT = 0
sumNTree (NodeT n t1 t2) = n + sumNTree t1 + sumNTree t2

sizeNTree :: NTree -> Integer
sizeNTree NilT = 0
sizeNTree (NodeT n t1 t2) = 1 + sizeNTree t1 + sizeNTree t2

depthNTree :: NTree -> Integer
depthNTree NilT = 0
depthNTree (NodeT n t1 t2) = 1 + max (depthNTree t1) (depthNTree t2)

-- note num times a number p occurs in tree
occursNTree :: NTree -> Integer -> Integer
occursNTree NilT p = 0
occursNTree (NodeT n t1 t2) p
    | n == p    = 1 + occursNTree t1 p + occursNTree t2 p
    | otherwise = occursNTree t1 p + occursNTree t2 p


collapseNTree :: NTree -> [Integer]
collapseNTree NilT = []
collapseNTree (NodeT n t1 t2) = collapseNTree t1 ++ [n] ++ collapseNTree t2
-----------------------------------------------------------------------------------------


-- exercise 44 --------------------------------------------------------------------------

-- data NTree = NilT | NodeT Integer NTree NTree
instance Arbitrary NTree where
    arbitrary = sized arbNTree
arbNTree 0 = return NilT --liftM NilT arbitrary
arbNTree n = frequency [(1, return NilT ),
                        (4, liftM3 NodeT arbitrary (arbNTree (n `div` 2))
                                                   (arbNTree (n `div` 2)) )]

propDepth :: NTree -> Bool
propDepth t = sizeNTree t < 2^(depthNTree t)




-- exercise 45 --------------------------------------------------------------------------

propOccurs :: NTree -> Integer -> Bool
propOccurs tree n = occursNTree tree n == fromIntegral (length (filter (== n) (collapseNTree tree)))




main = do
    quickCheckWith stdArgs {maxSuccess = 1000} propDepth
    quickCheckWith stdArgs {maxSuccess = 1000} propOccurs