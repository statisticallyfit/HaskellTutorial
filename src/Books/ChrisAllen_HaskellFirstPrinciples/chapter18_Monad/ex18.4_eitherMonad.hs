import Data.Monoid (Monoid, (<>))
import Control.Applicative
import Control.Monad (join)
import Test.QuickCheck (Arbitrary, arbitrary, elements, frequency)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative, monad)




{-
-- note takes result from operatoin on left side and puts it as arg to
    the operation on the right side

    (>>=) :: Monad m => m a -> (a -> m b) -> m b
-}



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
    (>>=) (Right' a) f = f a
    --(>>=) (Right' f) (Right' a) = join $ fmap (f a)


------------------------------------------------------------------------------
instance (Arbitrary e, Arbitrary a) => Arbitrary (Either' e a)  where
    arbitrary = do
        e <- arbitrary
        a <- arbitrary
        elements [Left' e, Right' a]

instance (Eq e, Eq a) => EqProp (Either' e a) where
    (=-=) = eq



{-

t1 = return 1 :: Either' String Int
t2 = (Left' "hi") (>>=) (Right' 1)
t3 = (Right' 1) (>>=) return . (+7)
-}


main = do
    {-print t1
    print t2
    print t3-}
    quickBatch $ applicative (undefined :: Either' String (Int,Int,Int))
    quickBatch $ monad (undefined :: Either' String (Int,Int,Int))