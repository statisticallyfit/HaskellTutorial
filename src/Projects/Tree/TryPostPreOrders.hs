import Data.Monoid
import qualified Data.Foldable as F

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

newtype InOrderTree a = In { getInOrderTree :: Tree a }
newtype PreOrderTree a = Pre { getPreOrderTree :: Tree a }
newtype PostOrderTree a = Post { getPostOrderTree :: Tree a }

instance F.Foldable Tree where
    foldMap f Empty        = mempty
    foldMap f (Node x l r) = F.foldMap f l <> (f x) <> F.foldMap f r

instance F.Foldable InOrderTree where
    foldMap f (InOrderTree Empty)        = mempty
    foldMap f (InOrderTree (Node x l r)) = F.foldMap f (Node x l r)

instance F.Foldable PreOrderTree where
    foldMap f (PreOrderTree Empty)        = mempty
    foldMap f (PreOrderTree (Node x l r)) = (f x) <> F.foldMap f l <> F.foldMap f r

instance F.Foldable PostOrderTree where
    foldMap f (PostOrderTree Empty)        = mempty
    foldMap f (PostOrderTree (Node x l r)) = F.foldMap f l <> F.foldMap f r <> f x

tree = Node 5
        (Node 3
           (Node 1 Empty Empty)
           (Node 6 Empty Empty))
        (Node 9
           (Node 8 Empty Empty)
           (Node 10 Empty Empty))


t1 = PostOrderTree tree
t2 = PreOrderTree tree
t3 = InOrderTree tree


sum = F.foldl (+) 0 t3
product = F.foldl (*) 1 t3

anyEqualTo n = getAny $ F.foldMap (\x -> Any (x == n)) t3

inOrderList = F.foldMap (\x -> [ x ]) t3
preOrderList = F.foldMap (\x -> [ x ]) t2
postOrderList = F.foldMap (\x -> [ x ]) t1