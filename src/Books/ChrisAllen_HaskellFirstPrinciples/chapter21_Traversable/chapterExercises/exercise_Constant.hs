import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show)


instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
    foldMap _ _ = mempty

instance Traversable (Constant a) where
    -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
    traverse _ (Constant x) = pure (Constant x)

------------------------------------------------------

-- note both arbitrary and eqprop have kind * so they need to take both a and b from
-- Constant

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
    arbitrary = do
        x <- arbitrary
        return (Constant x)


instance (Eq a, Eq b) => EqProp (Constant a b) where (=-=) = eq


main = do
    let trigger = undefined ::  Constant Int (Int, Char, [Char])
    quickBatch (traversable trigger)