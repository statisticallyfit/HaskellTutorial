
--- 16.7 SEARCH TREES ---------------------------------------------------------------------

{-
NOTE tree (Node val t1 t2) is ordered if
* all values in t1 are smaller than val
* all values in t2 are larger than val
* and trees t1 and t2 are ordered.

Going to construct functions that preserve order of our binary search tree.
-}

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq, Show)


t1 :: Tree Integer
t1 = Node 8 (Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil))
                    (Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil)))
            (Node 10 (Node 9 Nil Nil) (Node 11 Nil Nil))

t2 :: Tree Integer
t2 = Node 8 (Node 4 (Node 2 Nil (Node 3 Nil Nil))
                    (Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil)))
            (Node 10 (Node 9 Nil Nil) (Node 11 Nil Nil))

t123 = Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)
t567 = Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil)

-- deleting 3 from these trees.
testDelete1 = Node 2 (Node 1 Nil Nil) (Node 3 Nil (Node 4 Nil Nil))
testDelete2 = Node 1 (Node 0 Nil Nil) (Node 3 (Node 2 Nil Nil) Nil)
testDelete3 = Node 1 (Node 0 Nil Nil) (Node 3 (Node 2 Nil Nil) (Node 4 Nil Nil))

-- insTree and delete are not inverse functions.
insTree :: Ord a => a -> Tree a -> Tree a
insTree val Nil = Node val Nil Nil
insTree val (Node v t1 t2)
    | v == val = Node v t1 t2
    | val > v  = Node v t1 (insTree val t2)
    | val < v  = Node v (insTree val t1) t2

delete :: Ord a => a -> Tree a -> Tree a
delete val (Node v t1 t2)
    | val < v   = Node v (delete val t1) t2
    | val > v   = Node v t1 (delete val t2)
    | isNil t2  = t1 -- so in these leftover 3 tests (val == v)
    | isNil t1  = t2
    | otherwise = join t1 t2

minTree :: Ord a => Tree a -> Maybe a
minTree t
    | isNil t = Nothing
    | isNil t1 = Just v
    | otherwise = minTree t1
    where t1 = leftSub t
          v = treeVal t

-- note is auxiliary, not exported.
-- postcondition: all elements on left are smaller than those on right.
join :: Ord a => Tree a -> Tree a -> Tree a
join t1 t2 = Node miniVal t1 newTree
    where (Just miniVal) = minTree t2
          newTree = delete miniVal t2


nil :: Tree a
nil = Nil

isNil :: Tree a -> Bool
isNil Nil = True
isNil _ = False

isNode :: Tree a -> Bool
isNode Nil = False
isNode _ = True

leftSub :: Tree a -> Tree a
leftSub Nil = error "leftSub"
leftSub (Node _ t1 _) = t1

rightSub :: Tree a -> Tree a
rightSub Nil = error "rightSub"
rightSub (Node _ _ t2) = t2

treeVal :: Tree a -> a
treeVal Nil = error "treeVal"
treeVal (Node v _ _) = v


-- note return nth element of search tree.
-- HELP understand how the (n- tsize1 - 1) thingy works.
-- It can be used in situations like indexTree 3 t2 ==> 5.
indexTree :: Int -> Tree a -> a
indexTree n t
    | isNil t = error "indexTree"
    | n < tSize1 = indexTree n t1
    | n == tSize1 = v
    | otherwise = indexTree (n - tSize1 - 1) t2
    where v = treeVal t
          t1 = leftSub t
          t2 = rightSub t
          tSize1 = size t1

size :: Tree a -> Int
size t
    | isNil t = 0
    | otherwise = 1 + size (leftSub t) + size (rightSub t)
