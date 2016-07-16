{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Data.Maybe


data S n a = S (n a) a deriving (Eq, Show)


instance Functor n => Functor (S n) where
    fmap f (S na a) = S (fmap f na) (f a)


instance Foldable n => Foldable (S n) where
    foldMap f (S na a) = (foldMap f na) <> (f a)


instance Traversable n => Traversable (S n) where
    -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
    traverse f (S na a) = S <$> traverse f na <*> f a



------------------------------------------------------------
instance Arbitrary a => Arbitrary (S [] a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (S [a] b)

instance Eq a => EqProp (S [] a) where (=-=) = eq


main = do
    let trigger = undefined :: S [] (Int, String, [Int])
    quickBatch (traversable trigger)
