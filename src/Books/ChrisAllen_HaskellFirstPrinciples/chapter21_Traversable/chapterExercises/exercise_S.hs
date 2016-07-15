import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



data S n a = S (n a) a deriving (Eq, Show)


instance Functor (S n) where
    fmap f (S na a) = S na (f a)


instance Foldable (S n) where
    foldMap f (S na a) = f a
{-

instance Traversable n => Traversable (S n) where
    traverse f (S na a) = (S na) <$> f a-}
