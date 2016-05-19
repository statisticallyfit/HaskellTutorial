import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>))

-- Given a data type implement the Monoid instance.
-- Add Monoid constraints to type variables where needed


-- help: how do the args Four a b c d get passed in here: which is in what place?
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)


-- help why is it not
-- instance (Semigroup Bool) => Semigroup (BoolConj Bool) where
-- instead of:
instance Semigroup BoolConj where
    (BoolConj a) <> (BoolConj b) = BoolConj (a && b) --notesince no semigroup for bool

instance Arbitrary BoolConj where
    arbitrary = do
        a <- arbitrary
        elements [(BoolConj a), (BoolConj a)]
        -- help what does elements function mean? Where do you get them:
        -- elements, frequency, choose... all that work with arbitrary?


type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool




{-
*Main Test.QuickCheck> :{
*Main Test.QuickCheck| newtype BoolConj = BoolConj Bool
*Main Test.QuickCheck|     deriving (Eq, Show)
*Main Test.QuickCheck|
*Main Test.QuickCheck| instance Arbitrary BoolConj where
*Main Test.QuickCheck|     arbitrary = do
*Main Test.QuickCheck|         a <- arbitrary
*Main Test.QuickCheck|         elements [(BoolConj a), (BoolConj a)]
*Main Test.QuickCheck| :}
*Main Test.QuickCheck> sample (arbitrary :: Gen BoolConj)
BoolConj False
BoolConj False
BoolConj False
BoolConj False
BoolConj False
BoolConj True
BoolConj True
BoolConj True
BoolConj True
BoolConj False
BoolConj True

-}


main :: IO()
main = do
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    print ((BoolConj True) <> (BoolConj True))
    print $ (BoolConj True) <> (BoolConj False)