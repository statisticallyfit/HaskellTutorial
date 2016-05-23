{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Function



functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)




{-
note
:k IgnoreOne
IgnoreOne :: (* -> *) -> (* -> *) -> * -> * -> *
-}


data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)
    -- HELP why do we get to use f it was applied out like the 'a'?




type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool


-- HELP why do these run 100% even without using the functor instance??
main = do
    quickCheck $ \x -> functorIdentity (x :: [Int])
    quickCheck (functorIdentity :: [Int] -> Bool )
    quickCheck (functorCompose :: IntFC)

    {-
    NOTE testing
   fmap (*8) (IgnoringSomething  [1..3] [1..3])
   IgnoringSomething [1,2,3] [8,16,24]
    -}