import Data.Monoid (Monoid, (<>))
import Control.Applicative
import Control.Monad (join)
import Test.QuickCheck (Arbitrary, arbitrary, elements, frequency)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative, monad, functor)



data Nope a = NopeDotJpg deriving (Eq, Show)


instance Functor Nope where
    fmap _ (NopeDotJpg) = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    NopeDotJpg >>= f = NopeDotJpg


--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
    (=-=) = eq




t1 = return 1 :: Nope Int
t2 = NopeDotJpg <*> NopeDotJpg
t3 = NopeDotJpg >>= return . (+1)
t4 = fmap (+1) NopeDotJpg

main = do
    print t1
    print t2
    print t3
    print t4

    let trigger = undefined :: Nope (Int, String, Int) -- help meaning of types?
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
