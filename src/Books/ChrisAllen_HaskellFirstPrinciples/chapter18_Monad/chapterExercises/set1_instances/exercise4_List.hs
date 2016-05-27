import Data.Monoid (Monoid, (<>), Sum)
import Control.Applicative
import Control.Monad (join)
import Test.QuickCheck (Arbitrary, arbitrary, elements, frequency)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative, monad, functor)


{-
(>>=)    :: Monad m => m a        -> (a -> m b) -> m b
-}


newtype Sum a = Sum { getSum :: a } deriving (Eq, Show) -- need for tests
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
    mempty = Nil
    mappend x Nil = x
    mappend Nil x = x
    mappend (Cons x xs) ys = Cons x $ xs `mappend` ys
    --- HELP are the ys of type List?

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    (<*>) _ Nil = Nil
    (<*>) Nil _ = Nil
    (<*>) (Cons f fs) xs = fmap f xs <> (fs <*> xs)
    -- HELP why not: (<*>) (Cons f fs) (Cons x xs) = Cons (f x) (fs <*> xs)
    -- note because it isn't applying each function to each x-value
    -- help but how to write it so that it is clear that xs is type List?

instance Monad List where
    return = pure
    (>>=) Nil _ = Nil
    (>>=) (Cons x xs) f = (f x) <> (xs >>= f)
    -- NOTE HELP why do we need (<>) mappend here? Is it because it is the only
    -- way to put the content inside the structure List?

---------------------------------------------------------------------------------

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)


instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [(1, return Nil), (10, return (Cons x y))]
        -- HELP what is y? Shouldn't it be a List? How does arbitrary
        -- know to make it into a List?

instance Eq a => EqProp (List a) where
    xs =-= ys = xs' `eq` ys'
                where xs' = take' 3000 xs
                      ys' = take' 3000 ys


t1 = return 23 :: List Integer
t_mappend = (Cons (Sum 1) (Cons (Sum 2) (Cons (Sum 3) Nil))) `mappend`
            (Cons (Sum 1) (Cons (Sum 2) (Cons (Sum 3) Nil)))
t_functor = fmap (+3) (Cons 1 (Cons 2 (Cons 3 Nil)))
t_app = (Cons (+1) (Cons (+2) (Cons (+3) Nil))) <*> (Cons 1 (Cons 2 (Cons 3 Nil)))
t_monad = (Cons 1 (Cons 2 (Cons 3 Nil))) >>= return . (*8)

main = do
    print t1
    print t_mappend
    print t_functor
    print t_app
    print t_monad
    --let trigger = undefined :: List (Int, String, Int) -- help type meaning?
    --quickBatch $ functor trigger -- note they work!
    --quickBatch $ applicative trigger
    --quickBatch $ monad trigger