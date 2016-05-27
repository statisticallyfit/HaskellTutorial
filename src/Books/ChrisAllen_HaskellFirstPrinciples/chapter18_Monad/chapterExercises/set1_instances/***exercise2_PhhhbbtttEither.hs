import Prelude hiding (Left, Right)
import Data.Monoid (Monoid, (<>))
import Control.Applicative
import Control.Monad (join)
import Test.QuickCheck (Arbitrary, arbitrary, elements, frequency)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative, monad, functor)




{-
-- note takes result from operatoin on left side and puts it as arg to
    the operation on the right side

    (>>=) :: Monad m => m a -> (a -> m b) -> m b
-}



data PhhhbbtttEither e a = Left e | Right a deriving (Eq, Show)


instance Functor (PhhhbbtttEither e) where
    fmap _ (Left e)  = Left e
    fmap f (Right a) = Right (f a)


instance Applicative (PhhhbbtttEither e) where
    pure a = Right a
    (<*>) _ (Left e) = Left e
    (<*>) (Left e) _ = Left e
    (<*>) (Right f) (Right a) = Right (f a)

instance Monad (PhhhbbtttEither e) where
    return = pure
    (>>=) (Left e) _ = Left e
    (>>=) (Right a) f = f a
    -- IMPORTANT NOTE: takes the content out of the structure on the left and
    -- applies the function on the right to the content to return content
    -- with structure wrapped around it.

------------------------------------------------------------------------------
instance (Arbitrary e, Arbitrary a) => Arbitrary (PhhhbbtttEither e a)  where
    arbitrary = do
        e <- arbitrary
        a <- arbitrary
        elements [Left e, Right a]

instance (Eq e, Eq a) => EqProp (PhhhbbtttEither e a) where
    (=-=) = eq





{-
HELP FIX
t1 = return 1 :: PhhhbbtttEither String Int -- (Int,String,Int)
t2 = (Left "hi") (>>=) return . (+1)
t3 = (Right 1) (>>=) return . (+7)
-}




main = do
{-    print t1
    print t2
    print t3-}
    let trigger = undefined :: PhhhbbtttEither String (Int,String,Int)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger