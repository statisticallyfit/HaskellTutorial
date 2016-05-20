import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid)
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a





newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)


-- help why is it not
-- instance (Semigroup Bool) => Semigroup (BoolConj Bool) where
-- instead of:
instance Semigroup BoolConj where
    (BoolConj a) <> (BoolConj b) = BoolConj (a && b) --notesince no semigroup for bool


instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)


instance Arbitrary BoolConj where
    arbitrary = do
        a <- arbitrary
        elements [(BoolConj a), (BoolConj a)]


type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool



main :: IO()
main = do
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    print ((BoolConj True) <> (BoolConj True))
    print $ (BoolConj True) <> (BoolConj False)