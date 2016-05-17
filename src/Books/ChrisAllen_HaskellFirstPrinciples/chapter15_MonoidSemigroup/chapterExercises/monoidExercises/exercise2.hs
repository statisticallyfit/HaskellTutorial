import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid)
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck)

-- Given a data type implement the Monoid instance.
-- Add Monoid constraints to type variables where needed

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


newtype Identity a = Identity a
    deriving (Eq, Show)


-- help why wouldn't it have worked like this: instance Semigroup (Identity a) where
instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)


instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a) -- help what does this mean, how does it help quickCheck?


type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool
 -- help why does it need to take a String arg?


main :: IO()
main = do
    quickCheck (semigroupAssoc :: IdentityAssoc)
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)