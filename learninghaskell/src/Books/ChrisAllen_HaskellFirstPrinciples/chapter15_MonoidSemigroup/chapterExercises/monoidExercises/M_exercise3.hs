import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid)
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck)


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


data Two a b = Two a b
    deriving (Eq, Show)


-- help why wouldn't it have worked like this: instance Semigroup (Identity a) where
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two c d) = Two (a <> c) (b <> d) -- help meaning?

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty -- for both a and b mempties
    mappend = (<>)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b) -- help what does this mean, how does it help quickCheck?


type TwoAssoc = Two String Ordering -> Two String Ordering ->
                    Two String Ordering -> Bool
 -- help why does it need to take a String arg?


main :: IO()
main = do
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (monoidLeftIdentity :: Two String Ordering -> Bool)
    quickCheck (monoidRightIdentity :: Two String Ordering -> Bool)