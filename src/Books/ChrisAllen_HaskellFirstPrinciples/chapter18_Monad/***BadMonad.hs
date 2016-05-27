import Data.Monoid (Monoid, (<>))
import Control.Applicative
import Control.Monad (join)
import Test.QuickCheck (Arbitrary, arbitrary, elements, frequency)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative, monad, functor)



data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
    fmap f (CountMe n a) = CountMe n (f a) -- HELP why can't (f i)?
    -- note correct version
    -- fmap f (CountMe i a) = CountMe i (f a)
    -- note bad version
    -- fmap f (CountMe i a) = CountMe (i+1) (f a)

instance Applicative CountMe where
    -- note correct versoin # 1 -- preserves identity with Pure 1 so monad works
    pure a = CountMe 1 a
    CountMe n f <*> CountMe n' a = CountMe (n * n') (f a)
    -- note correct version # 2
    {-
    pure a = CountMe 0 a
    CountMe i f <*> CountMe i' a = CountMe (i + i') (f a)
    -}


-- HELP still doesn't pass all the tests!!!
instance Monad CountMe where
    return = pure
    -- note correct version # 1
    CountMe n a >>= f = let CountMe n' b = f a
                        in CountMe (n + n') b
                        --CountMe (n + n') b
                        --where (CountMe n' b) = f a
    {-CountMe i a >>= f = CountMe (i + 1) b
                        where (CountMe _ b) = f a
                        -}
                        -- note doesn't matter what integer part is, throw it away.

    -- note correct version # 2
    -- CountMe _ a >>= f = f a
    -- problem: pure returns 0 in Integer place, fails identity test for monads.

instance Arbitrary a => Arbitrary (CountMe a) where
    arbitrary = CountMe <$> arbitrary <*> arbitrary
    -- HELP meaning? what does it mean combining the arbitrary functions this way?

instance Eq a => EqProp (CountMe a) where
    (=-=) = eq




main = do
    let trigger = undefined :: CountMe (Int, String, Int) -- HELP type meaning?
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger