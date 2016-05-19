import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>))

-- Given a data type implement the Monoid instance.
-- Add Monoid constraints to type variables where needed


-- help: how do the args Four a b c d get passed in here: which is in what place?
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


newtype BoolDisj = BoolDisj Bool
    deriving (Eq, Show)


-- help why is it not
-- instance (Semigroup Bool) => Semigroup (BoolConj Bool) where
-- instead of:
instance Semigroup BoolDisj where
    (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b) --notesince no semigroup for bool

instance Arbitrary BoolDisj where
    arbitrary = do
        a <- arbitrary
        elements [(BoolDisj a), (BoolDisj a)]
        -- help what does elements function mean? Where do you get them:
        -- elements, frequency, choose... all that work with arbitrary?


-- help why don't you have to provide the a (Bool) argument for BoolDisj anymore?
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool




main :: IO()
main = do
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    print $ (BoolDisj True) <> (BoolDisj True)
    print $ (BoolDisj True) <> (BoolDisj False)