import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



data Option a b = None a | Some b deriving (Eq, Show)


instance Functor (Option a) where
    fmap _ (None x) = None x
    fmap f (Some y) = Some (f y)

instance Foldable (Option a) where
    foldMap _ (None x) = mempty
    foldMap f (Some y) = f y

    foldr _ z (None x) = z
    foldr f z (Some y) = f y z

    foldl _ z (None x) = z
    foldl f z (Some y) = f z y

instance Traversable (Option a) where
    -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
    traverse _ (None x) = None x
    traverse f (Some y) = Some <$> f y

------------------------------------------------------

-- note both arbitrary and eqprop have kind * so they need to take both a and b from
-- Constant

instance (Arbitrary a, Arbitrary b) => Arbitrary (Option a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [None x, Some y]


instance (Eq a, Eq b) => EqProp (Option a b) where (=-=) = eq


main = do
    let trigger = undefined ::  Option Int (Int, Char, [Char])
    quickBatch (traversable trigger)