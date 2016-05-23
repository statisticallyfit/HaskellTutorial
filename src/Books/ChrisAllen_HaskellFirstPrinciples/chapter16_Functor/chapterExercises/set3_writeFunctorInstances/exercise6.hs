{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Function



functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)



{-
:k Parappa
Parappa :: (* -> *) -> (* -> *) -> * -> *
-}

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)
    -- HELP why doesn't g get applied?



type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool


-- HELP why do these run 100% even without using the functor instance??
main = do
    quickCheck $ \x -> functorIdentity (x :: [Int])
    quickCheck (functorIdentity :: [Int] -> Bool )
    quickCheck (functorCompose :: IntFC)

    {-
    NOTE testing
    fmap (*2) (DaWrappa [1..3] [2..4])
    DaWrappa [2,4,6] [4,6,8]
    -}