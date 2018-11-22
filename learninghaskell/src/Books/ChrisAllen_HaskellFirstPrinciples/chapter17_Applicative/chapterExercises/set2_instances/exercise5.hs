import Data.Monoid (Monoid, (<>), Sum)
import Control.Applicative
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative)


-- NOTE HELP so :k measures the kind of the data not the constructor since
-- :k Three' ==> * -> * -> *
data Three' a b = Three' a b b deriving (Eq, Show)


instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
    pure x = Three' mempty x x
    (<*>) (Three' a f1 f2) (Three' a' b1 b2) = Three' (a <> a') (f1 b1) (f2 b2)




instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Three' a b b)

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq


t1 = pure 1 :: Three' String Int -- NOTE HELP only 2 types are given so we are using
-- the constructor, not the data? HELP understand better
t2 = (Three' "" (+1) (*8)) <*> (Three' "" 1 3)


main = do
    print t1
    print t2
    quickBatch $ applicative (undefined :: Three' String (Int,Char,Double))
    --quickBatch $ applicative (Three' ("b", "w", 1) ("b", "w", 1) ("b", "w", 1))
    --quickBatch $ applicative (Three' "b" ("f", "f", 4) ("c", "h", 2))
    -- HELP why don't these work but the first one does?