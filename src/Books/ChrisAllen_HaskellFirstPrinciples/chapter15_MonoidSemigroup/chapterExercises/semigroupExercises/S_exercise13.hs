import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))




-- help: how do the args Four a b c d get passed in here: which is in what place?
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)



data Validation a b = Failure a | Success b
    deriving (Eq, Show)



newtype AccumulateBoth a b = AccumulateBoth (Validation a b)
    deriving (Eq, Show)


-- help: why are both types 'a' and 'b' semigrouped while in the previous exercises
-- only one of them was semigrouped?
instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    (AccumulateBoth (Success a)) <> (AccumulateBoth (Success b)) =
        AccumulateBoth (Success (a <> b))
    (AccumulateBoth (Failure a)) <> (AccumulateBoth (Failure b)) =
        AccumulateBoth (Failure (a <> b))
    _ <> (AccumulateBoth (Failure a)) = AccumulateBoth (Failure a)
    (AccumulateBoth (Failure a)) <> _ = AccumulateBoth (Failure a)



instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [(AccumulateBoth (Success a)), (AccumulateBoth (Failure b))]
        -- help meaning?


type AccumulateBothAssoc = AccumulateBoth String Ordering ->
     AccumulateBoth String Ordering -> AccumulateBoth String Ordering -> Bool



main :: IO()
main = do
    quickCheck (semigroupAssoc :: AccumulateBothAssoc)