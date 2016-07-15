import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid


data Three a b c = Three a b c deriving (Eq, Show)



instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)


instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

    foldr f z (Three a b c) = f c z

    foldl f z (Three a b c) = f z c


-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
instance Traversable (Three a b) where
    traverse f (Three x y z) = (Three x y) <$> f z

------------------------------------------------------------------------


instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three x y z)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq




main :: IO()
main = do
    let trigger = undefined :: Three Int Int (Int, Int, [Int])
    quickBatch (traversable trigger)