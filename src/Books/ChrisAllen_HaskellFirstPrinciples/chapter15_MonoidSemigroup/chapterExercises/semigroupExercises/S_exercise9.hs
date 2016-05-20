import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))

-- Given a data type implement the Monoid instance.
-- Add Monoid constraints to type variables where needed


-- help: how do the args Four a b c d get passed in here: which is in what place?
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


newtype Combine a b = Combine { unCombine :: (a -> b) }
    deriving (Eq, Show)


f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)


instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
    Combine {unCombine=f} <> Combine {unCombine=g} = Combine (f <> g)

main = print ""


{- NOTE: 
class Coarbitrary a where
    coarbitrary :: a -> (Gen b -> Gen b)
I.e., coarbitrary takes a value of a and yields a generator
transformer that takes a b generator and yields a new b
generator whose behavior depends on the a argument.
-}