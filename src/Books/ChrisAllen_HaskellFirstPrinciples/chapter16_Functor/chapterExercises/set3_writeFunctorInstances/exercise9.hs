{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Function



functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)




{-
note
:k Notorious
Notorious :: (* -> *) -> * -> * -> * -> *
-}




data List a = Nil | Cons a (List a) deriving (Eq, Show)


instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a la) = Cons (f a) (fmap f la)
    --fmap f (Cons a (List a)) = (Cons (f a)) (fmap List a )

-- note have to use fmap on whatever is a functor, and here, List is a functor!




type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool


-- HELP why do these run 100% even without using the functor instance??
main = do
    quickCheck $ \x -> functorIdentity (x :: [Int])
    quickCheck (functorIdentity :: [Int] -> Bool )
    quickCheck (functorCompose :: IntFC)

    {-HELP how to test this.
    -}