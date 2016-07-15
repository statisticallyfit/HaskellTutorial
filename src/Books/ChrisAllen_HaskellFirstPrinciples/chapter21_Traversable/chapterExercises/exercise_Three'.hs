import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid


data Three a b = Three a b b deriving (Eq, Show)



instance Functor (Three a) where
    fmap f (Three x y z) = Three x (f y) (f z)


instance Foldable (Three a) where
    foldMap f (Three a b c) = f b <> f c

    foldr f z (Three a b c) = b `f` (c `f` z)

    foldl f z (Three a b c) = (z `f` b) `f` c


-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
instance Traversable (Three a) where
    traverse f (Three x y z) = (Three x) <$> f y <*> f z
    -- note this becomes (Three x (f y)) <*> (f z) = (Three x (f y) (f z))


------------------------------------------------------------------------


instance (Arbitrary a, Arbitrary b) => Arbitrary (Three a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three x y z)

instance (Eq a, Eq b) => EqProp (Three a b) where (=-=) = eq




main :: IO()
main = do
    let trigger = undefined :: Three Int (Int, Int, [Int])
    quickBatch (traversable trigger)