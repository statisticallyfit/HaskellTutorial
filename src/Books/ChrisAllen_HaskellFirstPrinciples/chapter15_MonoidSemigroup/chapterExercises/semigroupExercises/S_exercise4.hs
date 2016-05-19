import Test.QuickCheck (Arbitrary, arbitrary, quickCheck)
import Data.Semigroup (Semigroup, (<>))

-- Given a data type implement the Monoid instance.
-- Add Monoid constraints to type variables where needed

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


data Three a b c = Three a b c
    deriving (Eq, Show)


instance (Semigroup a,
          Semigroup b,
          Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three e f g) = Three (a <> e) (b <> f) (c <> g)


-- help: find out what this is supposed to mean, assigning arbitrary to a,b,c
instance (Arbitrary a,
          Arbitrary b,
          Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)


type ThreeAssoc = Three String Ordering String -> Three String Ordering String ->
                  Three String Ordering String -> Bool
-- help why these types for a b c? Why not any other types?


main :: IO()
main = quickCheck (semigroupAssoc :: ThreeAssoc)