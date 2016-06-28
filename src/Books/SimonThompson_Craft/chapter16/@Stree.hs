import Test.QuickCheck
import Control.Monad hiding (join)


data Stree a = Nil | Node a Int (Stree a) (Stree a)
    deriving (Eq, Show)




t1 :: Stree Integer
t1 = Node 8 11 (Node 4 7 (Node 2 3 (Node 1 1 Nil Nil) (Node 3 1 Nil Nil))
                         (Node 6 3 (Node 5 1 Nil Nil) (Node 7 1 Nil Nil)))
               (Node 10 3 (Node 9 1 Nil Nil) (Node 11 1 Nil Nil))

t2 :: Stree Integer
t2 = Node 8 10 (Node 4 6 (Node 2 2 Nil (Node 3 1 Nil Nil))
                         (Node 6 3 (Node 5 1 Nil Nil) (Node 7 1 Nil Nil)))
            (Node 10 3 (Node 9 1 Nil Nil) (Node 11 1 Nil Nil))

t3 = Node 9 10 (Node 4 7 (Node 2 3 (Node 1 1 Nil Nil) (Node 3 1 Nil Nil))
                         (Node 6 3 (Node 5 1 Nil Nil) (Node 7 1 Nil Nil)) )
               (Node 10 2 Nil (Node 11 1 Nil Nil))

t123 = Node 2 3 (Node 1 1 Nil Nil) (Node 3 1 Nil Nil)
t567 = Node 6 3 (Node 5 1 Nil Nil) (Node 7 1 Nil Nil)

-- deleting 3 from these trees.
testDelete1 = Node 2 4 (Node 1 1 Nil Nil) (Node 3 2 Nil (Node 4 1 Nil Nil))
testDelete2 = Node 1 4 (Node 0 1 Nil Nil) (Node 3 2 (Node 2 1 Nil Nil) Nil)
testDelete3 = Node 1 5 (Node 0 1 Nil Nil) (Node 3 3 (Node 2 1 Nil Nil) (Node 4 1 Nil Nil))

-- testing deleting same item
testDelete4 = Node 2 3 (Node 2 1 Nil Nil) (Node 13 1 Nil Nil)



insTree :: Ord a => a -> Stree a -> Stree a
insTree val Nil = Node val 1 Nil Nil
insTree val (Node v n t1 t2)
    | v == val = Node v n t1 t2
    | val > v  = Node v (1 + size t1 + size nt2) t1 nt2
    | val < v  = Node v (1 + size nt1 + size t2) nt1 t2
    where
    nt1 = insTree val t1
    nt2 = insTree val t2

numOccurs :: Eq a => a -> Stree a -> Int
numOccurs val Nil = 0
numOccurs val (Node v _ t1 t2)
    | val == v = 1 + numoccs
    | otherwise = numoccs
    where numoccs = numOccurs val t1 + numOccurs val t2

occurs :: Ord a => a -> Stree a -> Bool
occurs val Nil = False
occurs val (Node v _ t1 t2)
    | val == v = True
    | val < v = occurs val t1
    | otherwise = occurs val t2

delete :: Ord a => a -> Stree a -> Stree a
delete val (Node v n t1 t2)
    | val < v   = Node v (n-1) (delete val t1) t2
    | val > v   = Node v (n-1) t1 (delete val t2)
    | isNil t2  = t1 -- so in these leftover 3 tests (val == v)
    | isNil t1  = t2
    | otherwise = join t1 t2

minTree :: Ord a => Stree a -> Maybe a
minTree t
    | isNil t = Nothing
    | isNil t1 = Just v
    | otherwise = minTree t1
    where t1 = leftSub t
          v = treeVal t

-- note is auxiliary, not exported.
-- precondition: all elements on left are smaller than those on right.
join :: Ord a => Stree a -> Stree a -> Stree a
join t1 t2 = Node miniVal n t1 newTree
    where (Just miniVal) = minTree t2
          newTree = delete miniVal t2
          n = size t1 + size newTree + 1 -- note plus 1 for this node's size.


isNil :: Stree a -> Bool
isNil Nil = True
isNil _ = False

isNode :: Stree a -> Bool
isNode Nil = False
isNode _ = True

leftSub :: Stree a -> Stree a
leftSub Nil = error "leftSub"
leftSub (Node _ _ t1 _) = t1

rightSub :: Stree a -> Stree a
rightSub Nil = error "rightSub"
rightSub (Node _ _ _ t2) = t2

treeVal :: Stree a -> a
treeVal Nil = error "treeVal"
treeVal (Node v _ _ _) = v


-- note return nth element of search tree.
-- HELP understand how the (n- tsize1 - 1) thingy works.
-- It can be used in situations like indexTree 3 t2 ==> 5.
indexTree :: Int -> Stree a -> a
indexTree n t
    | isNil t = error "indexTree"
    | n < tSize1 = indexTree n t1
    | n == tSize1 = v
    | otherwise = indexTree (n - tSize1 - 1) t2
    where v = treeVal t
          t1 = leftSub t
          t2 = rightSub t
          tSize1 = size t1

-- this Stree implementation is more efficient because size is not recursive.

size :: Stree a -> Int
size Nil = 0
size (Node _ n _ _) = n



sizeRecursive :: Stree a -> Int
size t
    | isNil t = 0
    | otherwise = 1 + size (leftSub t) + size (rightSub t)
















--- TESTING ------------------------------------------------------------------------------



instance Arbitrary a => Arbitrary (Stree a) where
    arbitrary = frequency [(1, return Nil), (4,liftM4 Node arbitrary arbitrary
                                                           arbitrary arbitrary)]
        {-x <- arbitrary
        Positive n <- arbitrary
        st1 <- arbitrary
        st2 <- arbitrary
        oneof [return Nil, return (liftM4 Node x n st1 st2)]
-}


propIns :: Integer -> Stree Integer -> Bool
propIns val t = size t + 1 == (size newTree)
    where newTree = insTree val t

------------------------------------------------------------------------------------------
