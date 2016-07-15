import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



data Option a = None | Some a deriving (Eq, Show)


instance Functor Option where
    fmap _ None     = None
    fmap f (Some x) = Some (f x)

instance Foldable Option where
    foldMap _ None     = mempty
    foldMap f (Some x) = f x

    foldr _ z None     = z
    foldr f z (Some x) = f x z

    foldl _ z None     = z
    foldl f z (Some x) = f z x

instance Traversable Option where
    -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
    traverse _ None     = pure (None)
    traverse f (Some x) = Some <$> f x

------------------------------------------------------

-- note both arbitrary and eqprop have kind * so they need to take both a and b from
-- Constant

instance (Arbitrary a) => Arbitrary (Option a) where
    arbitrary = do
        x <- arbitrary
        elements [None, Some x]


instance Eq a => EqProp (Option a) where (=-=) = eq



-- HELP why cant we just put Option Int why do we have to write a threeple?
main = do
    let trigger = undefined ::  Option (Int, Char, [Char])
    quickBatch (traversable trigger)