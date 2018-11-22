import Test.QuickCheck (Arbitrary, arbitrary, quickCheck)
import Data.Semigroup (Semigroup, (<>))

-- Given a data type implement the Monoid instance.
-- Add Monoid constraints to type variables where needed


-- help: how do the args Four a b c d get passed in here: which is in what place?
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


data Four a b c d = Four a b c d
    deriving (Eq, Show)


instance (Semigroup a,
          Semigroup b,
          Semigroup c,
          Semigroup d) => Semigroup (Four a b c d) where
    (Four a b c d) <> (Four e f g h) = Four (a <> e)(b <> f)(c <> g)(d <> h)


instance (Arbitrary a,
          Arbitrary b,
          Arbitrary c,
          Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return (Four a b c d)



type FourAssoc = Four String Ordering String Ordering ->
                 Four String Ordering String Ordering ->
                 Four String Ordering String Ordering -> Bool



main :: IO()
main = quickCheck (semigroupAssoc :: FourAssoc)