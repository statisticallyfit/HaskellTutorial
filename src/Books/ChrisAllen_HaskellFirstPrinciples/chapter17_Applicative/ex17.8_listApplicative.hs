import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



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

-- this is what we are doing in the applicative <*>
-- take list of functions and apply each to a list of values to return
-- a list of lists
applyToAll :: List (a -> b) -> List a -> List (List b)
applyToAll Nil _ = Nil
applyToAll _ Nil = Nil
applyToAll (Cons f fs) xs = Cons (fmap f xs) (applyToAll fs xs )




data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
    mempty = Nil
    mappend x Nil = x
    mappend Nil x = x
    mappend (Cons x xs) ys = Cons x $ xs `mappend` ys

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil -- HELP why is this the same as pure = const Nil ?
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (Cons f fs) <*> xs = fmap f xs <> (fs <*> xs)



instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Cons x (Cons y Nil))

-- HELP understand how this is used in testing
-- note used List a since EqProp has kind (*) and List a has kind * -> *
instance Eq a => EqProp (List a) where (=-=) = eq



-- Test values
functions = Cons (+1) (Cons (*2) Nil)
values = Cons 1 (Cons 2 Nil)
fs1 = Cons (+1) (Cons (*3) (Cons (+2) Nil))
vs1 = Cons   1  (Cons   5  (Cons   8  Nil))

fs2 = Cons (+1) (Cons (*3) Nil)
vs2 = vs1

main = do
    print $ functions <*> values
    print $ fs1 <*> vs1
    print $ fs2 <*> vs2

    --quickBatch $ applicative ( [("b", "w", 1)] :: [(String, String, Int)])
    --quickBatch $ applicative ((Just ("b", "w", 1)) :: (Maybe (String, String, Int)))
    --quickBatch $ applicative ((Cons ("b", "w", 1) Nil) :: (List (String, String, Int)))
