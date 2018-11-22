
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)


instance Applicative Identity where
    pure x = Identity x
    (Identity f) <*> (Identity y) = Identity (f y)


instance Foldable Identity where
    foldMap f (Identity x) = f x
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x


instance Traversable Identity where
    -- note apply function on x then fmap the Identity inside x.
    -- example: Identity <$> Right 1 = Right (Identity 1)
    -- which is the same as (traverse Right (Identity 1))
    traverse f (Identity x) = Identity <$> f x


---------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

-- help why do we need edprop? Works without it?...
instance Eq a => EqProp (Identity a) where (=-=) = eq



main = do
    let trigger = undefined :: Identity (Int, Int, [Int])
    quickBatch (traversable trigger)