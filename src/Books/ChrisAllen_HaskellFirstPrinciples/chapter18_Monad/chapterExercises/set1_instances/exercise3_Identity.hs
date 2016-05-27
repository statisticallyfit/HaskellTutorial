import Data.Monoid (Monoid, (<>))
import Control.Applicative
import Control.Monad (join)
import Test.QuickCheck (Arbitrary, arbitrary, elements, frequency)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative, monad, functor)


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
    return = pure
    (>>=) (Identity a) f = f a


--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)


instance Eq a => EqProp (Identity a) where
    (=-=) = eq



--HELP Fix
t1 = return 1 :: Identity Int
t2 = Identity (+4) <*> Identity 1
t3 = Identity 3 >>= return . (+5)




main = do
    print t1
    print t2
    print t3
    let trigger = undefined :: Identity (Int, String, Int)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger