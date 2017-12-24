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




data GoatBud a = NoGoat
                    | OneGoat a
                    | MoreGoats (GoatBud a) (GoatBud a) (GoatBud a)
                    deriving (Eq, Show)


instance Functor GoatBud where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)



type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool


-- HELP why do these run 100% even without using the functor instance??
main = do
    quickCheck $ \x -> functorIdentity (x :: [Int])
    quickCheck (functorIdentity :: [Int] -> Bool )
    quickCheck (functorCompose :: IntFC)


    {-
    NOTE testing
    *Main> let g1 = OneGoat 1
    *Main> let g2 = OneGoat 2
    *Main> let g3 = OneGoat 3
    *Main> fmap (+1) (MoreGoats g1 g2 g3)
    MoreGoats (OneGoat 2) (OneGoat 3) (OneGoat 4)
    -}