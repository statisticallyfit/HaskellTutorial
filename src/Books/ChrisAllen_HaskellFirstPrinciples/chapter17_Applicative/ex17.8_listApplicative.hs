import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data List a = Nil | Cons a (List a) deriving (Eq, Show)




append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ seed Nil = seed
fold f seed (Cons h t) = f h (fold f seed t)
-- h = head, t = tail

concat' :: List (List a) -> List a
concat' lol = fold append Nil lol

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as


{-
instance Monoid (List a) where
    mempty = Nil
    mappend a Nil = a
    mappend Nil a = a
    mappend (Cons x xs) ys = Cons x $ xs `mappend` ys
-}
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = (Cons f x) (fmap f xs)
{-
instance Applicative List where
    pure x = Cons x Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f b) ca = fmap f ca <> (b <*> ca) -- HELP




instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Cons x (Cons y Nil))

-- HELP understand how this is used in testing
-- note used List a since EqProp has kind (*) and List a has kind * -> *
instance Eq a => EqProp (List a) where (=-=) = eq


main = do
    quickBatch $ applicative ( [("b", "w", 1)] :: [(String, String, Int)])
    quickBatch $ applicative ((Just ("b", "w", 1)) :: (Maybe (String, String, Int)))
    quickBatch $ applicative ((Cons ("b", "w", 1) Nil) :: (List (String, String, Int)))
-}