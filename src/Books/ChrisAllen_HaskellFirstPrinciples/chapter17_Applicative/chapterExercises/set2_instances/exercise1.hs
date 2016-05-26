import Data.Monoid (Monoid, (<>))
import Control.Applicative
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative)


newtype Identity a = Identity a deriving (Eq, Show)


instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) x = fmap f x


instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)

instance Eq a => EqProp (Identity a) where
    (=-=) = eq


main = do
    quickBatch $ applicative (undefined :: Identity(String,String,Int))
    --quickBatch $ applicative ((Identity ("b", "w", 1)):: Identity (String,String,Int))
