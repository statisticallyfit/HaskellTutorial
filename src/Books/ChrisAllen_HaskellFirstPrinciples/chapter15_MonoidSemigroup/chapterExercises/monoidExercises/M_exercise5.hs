import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid)
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a




newtype BoolDisj = BoolDisj Bool
    deriving (Eq, Show)


-- help why is it not
-- instance (Semigroup Bool) => Semigroup (BoolConj Bool) where
-- instead of:
instance Semigroup BoolDisj where
    (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b) --notesince no semigroup for bool


instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)


instance Arbitrary BoolDisj where
    arbitrary = do
        a <- arbitrary
        elements [(BoolDisj a), (BoolDisj a)]


-- help why don't you have to provide the a (Bool) argument for BoolDisj anymore?
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool




main :: IO()
main = do
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    print $ (BoolDisj True) <> (BoolDisj True)
    print $ (BoolDisj True) <> (BoolDisj False)