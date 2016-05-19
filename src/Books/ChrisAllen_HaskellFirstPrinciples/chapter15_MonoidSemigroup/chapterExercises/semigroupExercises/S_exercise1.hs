import Test.QuickCheck (Arbitrary, arbitrary, quickCheck)
import Data.Semigroup (Semigroup, (<>))

-- Given a data type implement the Monoid instance.
-- Add Monoid constraints to type variables where needed

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)



data Trivial = Trivial
    deriving (Eq, Show)



-- help: why can't this be written (<>) = Trivial?

instance Semigroup Trivial where
    _ <> _ = Trivial


-- note: this is for the quickcheck tester
instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool


main :: IO()
main =
    quickCheck (semigroupAssoc :: TrivialAssoc)
