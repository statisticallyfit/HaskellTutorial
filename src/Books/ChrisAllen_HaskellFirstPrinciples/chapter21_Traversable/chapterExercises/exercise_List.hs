import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid


data List a = Nil | Cons a (List a) deriving (Eq, Show)



instance Monoid (List a) where
    mempty = Nil
    mappend x Nil = x
    mappend Nil x = x
    mappend (Cons x xs) cs = (Cons x) (xs <> cs)


instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x list) = Cons (f x) (fmap f list)


instance Applicative List where
    pure x = Cons x Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f fs) cs@(Cons x xs) = fmap f cs <> (fs <*> cs)
-- NOTE IMPORTANT need mappend operator (<>) since result of (fmap f xs) is
-- a list: Cons a (Cons a1 (Cons a2 ...)) and we need to mappend this
-- to the result of (fs <*> xs)


instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons x list) = (f x) <> (foldMap f list)

    foldr _ z Nil = z
    foldr f z (Cons x list) = foldr f (f x z) list

    foldl _ z Nil = z
    foldl f z (Cons x list) = foldl f (f z x) list


-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons x list) = Cons <$> (f x) <*> traverse f list

------------------------------------------------------------------------


instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Cons x (Cons y Nil))

instance Eq a => EqProp (List a) where (=-=) = eq


fs = Cons (+1) (Cons (+2) (Cons (+5) Nil))
vs = Cons 1 (Cons 2 (Cons 3 Nil))



main :: IO()
main = do
    -- foldable
    print $ foldr (-) (-8) (Cons 1 (Cons 2 (Cons (-5) Nil)))
    print $ foldl (-) (-8) (Cons 1 (Cons 2 (Cons (-5) Nil)))
    -- applicative
    print $ fs <*> vs

    let trigger = undefined :: List (Int, Int, [Int])
    quickBatch (traversable trigger)

