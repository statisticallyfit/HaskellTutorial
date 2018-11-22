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

{-# LANGUAGE FlexibleInstance #-}


data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
    fmap g (Notorious go ga gt) = Notorious go ga (fmap g gt)
    -- note could also use letter (f) instead of (g)





type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool


-- HELP why do these run 100% even without using the functor instance??
main = do
    quickCheck $ \x -> functorIdentity (x :: [Int])
    quickCheck (functorIdentity :: [Int] -> Bool )
    quickCheck (functorCompose :: IntFC)

    {-
    HELP testing
    why does this not work
    fmap (-1) (Notorious (Just 1) (Just 2) (Just 3))

    *Main> fmap (*3) (Notorious [1..3] [1..3] [1..3])
    Notorious [1,2,3] [1,2,3] [3,6,9]
    -}