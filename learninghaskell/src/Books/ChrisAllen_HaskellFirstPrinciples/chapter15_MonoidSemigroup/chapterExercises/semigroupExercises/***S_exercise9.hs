import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))

-- Given a data type implement the Monoid instance.
-- Add Monoid constraints to type variables where needed


-- help: how do the args Four a b c d get passed in here: which is in what place?
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)




-- NOTE: newtypes have only a single constructor with exactly one argument.
-- more on newtypes here:
-- http://degoes.net/articles/newtypes-suck
-- NOTE: also look at LearnHaskellGreatGood on pdf page 200.

newtype Combine a b = Combine { unCombine :: (a -> b)} deriving (Eq, Show)





-- help note: it might be semigroup b and not semigroup a and b since
-- the argumen \n is type a and Sum(n+-1) is type b so they are diffferent
-- types and Sum is a Semigroup (as well as a Monoid) while a is not Semigroup.
instance Semigroup b => Semigroup (Combine a b) where
    Combine {unCombine = f} <> Combine {unCombine = g} = Combine (f <> g)
    --(Combine f) <> (Combine g) = Combine (f <> g)




f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)




type CombineAssoc = Combine String Ordering -> Combine String Ordering ->
                    Combine String Ordering -> Bool




--main = quickCheck (semigroupAssoc :: CombineAssoc)
main = do
    print $ unCombine (f <> g) $ 0


-- HELP HELP HELP HELP how to do the arbitrary, coarbitrary thing.
{-
instance (Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [(Combine a b), (Combine a b)]
-}
{- NOTE:
class Coarbitrary a where
    coarbitrary :: a -> (Gen b -> Gen b)
I.e., coarbitrary takes a value of a and yields a generator
transformer that takes a b generator and yields a new b
generator whose behavior depends on the a argument.
-}