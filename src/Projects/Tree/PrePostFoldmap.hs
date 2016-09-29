
-- source of this code:
-- http://www.afronski.pl/7-languages-in-7-weeks/2015/08/26/seven-languages-in-seven-weeks-haskell.html


import Data.Monoid
import qualified Data.Foldable as F

data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

newtype InOrderTree a = In { getInOrderTree :: Tree a }
newtype PreOrderTree a = Pre { getPreOrderTree :: Tree a }
newtype PostOrderTree a = Post { getPostOrderTree :: Tree a }
{-

instance F.Foldable Tree where
    foldMap f Empty        = mempty
    foldMap f (Leaf x)     = f x
    foldMap f (Node x l r) = foldMap f l <> (f x) <> foldMap f r

instance F.Foldable InOrderTree where
    foldMap f (In Empty)        = mempty
    foldMap f (In (Leaf x))     = f x
    foldMap f (In (Node x l r)) = foldMap f (Node x l r)

instance F.Foldable PreOrderTree where
    foldMap f (Pre Empty)        = mempty
    foldMap f (Pre (Leaf x))     = f x
    foldMap f (Pre (Node x l r)) = (f x) <> foldMap f l <> foldMap f r

instance F.Foldable PostOrderTree where
    foldMap f (Post Empty)        = mempty
    foldMap f (Post (Leaf x))     = f x
    foldMap f (Post (Node x l r)) = foldMap f l <> foldMap f r <> f x
-}


instance F.Foldable Tree where
    foldMap f Empty        = mempty
    foldMap f (Leaf x)     = f x
    foldMap f (Node x l r) = foldMap f l <> (f x) <> foldMap f r

instance F.Foldable InOrderTree where
    foldr f z (In tree) = go tree z
        where
        go Empty z        = z
        go (Leaf x) z     = f x z
        go (Node x l r) z = let z'   = go l z    -- left subtree
                                z''  = f x z'    -- current node
                                z''' = go r z''  -- right subtree
                            in z'''

instance F.Foldable PreOrderTree where
    foldr f z (Pre tree) = go tree z
        where
        go Empty z        = z
        go (Leaf x) z     = f x z
        go (Node x l r) z = let z'   = f x z
                                z''  = go l z'
                                z''' = go r z''
                            in z'''

instance F.Foldable PostOrderTree where
    foldr f z (Post tree) = go tree z
        where
        go Empty z        = z
        go (Leaf x) z     = f x z
        go (Node x l r) z = let z'   = go l z    -- left subtree
                                z''  = go r z'  -- right subtree
                                z''' = f x z''    -- current node
                            in z'''

tree = Node 5
        (Node 3
           (Leaf 1)
           (Leaf 6))
        (Node 9
           (Leaf 8)
           (Leaf 10))


tree15 = Node 8
            (Node 4
                (Node 2 (Leaf 1) (Leaf 3))
                (Node 5 (Leaf 6) (Leaf 7)))
            (Node 12
                (Node 10 (Leaf 9) (Leaf 11))
                (Node 14 (Leaf 13) (Leaf 15)))
tree7 = Node 7
            (Node 1
                (Leaf 0)
                (Node 3
                    (Leaf 2)
                    (Node 5 (Leaf 4) (Leaf 6))))
            (Node 9
                (Leaf 8) (Leaf 10))


postTree = Post tree7
preTree = Pre tree7
inTree = In tree7


sum = F.foldl (+) 0 inTree
product = F.foldl (*) 1 inTree

anyEqualTo n = getAny $ F.foldMap (\x -> Any (x == n)) inTree



---------------------------------------------------------------------
-- NOTE the only thing different is where the root node is placed!
-- otherwise these are the same, or inorder from root down, exclusive.
foldMapIn = F.foldMap (\x -> [ x ]) inTree
foldMapPre = F.foldMap (\x -> [ x ]) preTree
foldMapPost = F.foldMap (\x -> [ x ]) postTree

foldrPost = reverse $ foldr (:) [] postTree
foldrPre = reverse $ foldr (:) [] preTree
foldrIn = reverse $ foldr (:) [] inTree

foldlPost = foldl (flip (:)) [] postTree
foldlPre = foldl (flip (:)) [] preTree
foldlIn = foldl (flip (:)) [] inTree