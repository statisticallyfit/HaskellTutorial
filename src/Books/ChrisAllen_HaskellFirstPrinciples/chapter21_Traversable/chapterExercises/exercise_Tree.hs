import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid



data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
    deriving (Eq, Show)


t1 = Node (Leaf 1) 8 (Node (Leaf 7) 9 (Leaf 10))
t2 = Node (Node (Node Empty 2 (Leaf 1)) 6 (Node (Leaf 8) 1 Empty)) 5 t1
t3 = Node (Leaf 6) 2 (Leaf 5)
t4 = Node (Leaf 1) 2 (Node (Leaf 3) 4 (Leaf 5))


instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)


instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node left a right) = (foldMap f left) <> (f a) <> (foldMap f right)

    foldl _ z Empty = z
    foldl f z (Leaf a) = f z a
    foldl f z (Node left a right) = foldl f (foldl f (f z a) left) right

    foldr _ z Empty = z
    foldr f z (Leaf a) = f a z
    foldr f z (Node left a right) = foldr f (f a (foldr f z right)) left



