import Data.Monoid (Monoid, (<>))
import Control.Applicative
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative)


-- note: todo: come back to do this once I cover previous chapters too:
-- https://lukleh.github.io/haskell-book-exercises/#_18_4_examples_of_monad_use


data Either' e a = Left' e | Right' a deriving (Eq, Show)


instance Functor (Either' e) where
    fmap _ (Left' e)  = Left' e
    fmap f (Right' a) = Right' (f a)


instance Applicative (Either' e) where
    pure a = Right' a
    (<*>) _ (Left' e) = Left' e
    (<*>) (Left' e) _ = Left' e
    (<*>) (Right' f) (Right' a) = Right' (f a)

instance Monad (Either' e) where
    return = pure
    (>>=) _ (Left' e) = Left' e
    (>>=) (Left' e) _ = Left' e
    (>>=) (Right' f) (Right' a) = join $ fmap (f a)


------------------------------------------------------------------------------
instance (Arbitrary e, Arbitrary a) => Arbitrary (Either' e a)  where
    arbitrary = frequency [(1, Left' <$> arbitrary),
                           (3, Right' <$> arbitrary)]

instance (Eq e, Eq a) => EqProp (Either' e a) where
    (=-=) = eq



main = do
    print $ ""