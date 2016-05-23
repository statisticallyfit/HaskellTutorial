{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Function



functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)





data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

-- HELP understand this type instance better
instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)


type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool


-- HELP why do these run 100% even without using the functor instance??
main = do
    quickCheck $ \x -> functorIdentity (x :: [Int])
    quickCheck (functorIdentity :: [Int] -> Bool )
    quickCheck (functorCompose :: IntFC)

    {-
    NOTE testing
     fmap (*3) (LiftItOut [3..9])
    LiftItOut [9,12,15,18,21,24,27]
    -}