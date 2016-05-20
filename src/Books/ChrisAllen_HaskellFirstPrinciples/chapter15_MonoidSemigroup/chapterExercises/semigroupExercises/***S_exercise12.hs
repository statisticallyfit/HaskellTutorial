import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))




-- help: how do the args Four a b c d get passed in here: which is in what place?
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)



data Validation a b = Failure a | Success b
    deriving (Eq, Show)


newtype AccumulateRight a b = AccumulateRight (Validation a b)
    deriving (Eq, Show)


-- help why does success get both a b types when it just has type b in definition?
{-
help why is validation defined differently than:
instance Semigroup a => Semigroup (Validation a b) where
    (Failure a) <> (Failure b) = Failure (a <> b)
    (Failure a) <> _           = Failure a
    _           <> (Failure a) = Failure a
    a           <> _           = a

I thought the previous is failure-oriented but why is this one below success-oriented?
-}
instance Semigroup b => Semigroup (AccumulateRight a b) where
    (AccumulateRight (Success a)) <> (AccumulateRight (Success b)) =
        AccumulateRight (Success (a <> b))
    (AccumulateRight (Failure a)) <> _ = AccumulateRight (Failure a)
    _ <> (AccumulateRight (Failure a)) = AccumulateRight (Failure a)



-- HELP: why does failure get b and success get a when the data definition says
-- their types are success b and failure a not the other way around?
instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [(AccumulateRight (Success a)), (AccumulateRight (Failure b))]


-- HELP why string /ordering, why not other types?
type AccumulateRightAssoc = AccumulateRight String Ordering ->
     AccumulateRight String Ordering -> AccumulateRight String Ordering -> Bool


main :: IO()
main = do
    quickCheck (semigroupAssoc :: AccumulateRightAssoc)