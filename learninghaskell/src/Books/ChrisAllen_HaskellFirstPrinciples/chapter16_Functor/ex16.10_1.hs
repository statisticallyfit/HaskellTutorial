import Test.QuickCheck
import Test.QuickCheck.Function


-- The properties -------------------------------------------------------------------
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

--functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
--functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-------------------------------------------------------------------------------------



-- 1
newtype Identity a = Identity a

-- note kind is already * -> * so we don't need to bring the 'a' up here.
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)


-- HELP what does this [Int] type mean in this context? Why use this particular type?
-- HELP is f the []? What is f and what is 'a' in [Int]?
type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool


-- HELP why need to give these particular tests? Why different identity tests?




-- 2
data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)






-- 3
data Two a b = Two a b

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)






-- 4
data Three a b c = Three a b c
{-
note
:k Three
Three :: * -> * -> * -> *
-}
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)




-- 5
data Three' a b = Three' a b b
-- HELP why
{-
note
:k Three'
Three' :: * -> * -> *  HELP is it a -> b -> return value?
Does the kind follow the kind of the data or constructor?
-}

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)
    -- HELP why should it be given 3 different args if the data takes only 2?
    -- HELP what does it mean that the constructor takes 3 args but 2 are the same?





-- 6
{-
note:
:k Four
Four :: * -> * -> * -> * -> *
-}
data Four a b c d = Four a b c d

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)





-- 7
{-
note:
:k Four'
Four' :: * -> * -> *

-}
data Four' a b = Four' a a a b

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)
-- HELP why does there have to be application to only the last value
-- when we are just taking out the first value (in the type of the instance)?
-- HELp why can't it be:
-- fmap f (Four' a b c d) = Four' a (f b) (f c) (f d)







main = do
    quickCheck $ \x -> functorIdentity (x :: [Int])
    quickCheck (functorIdentity :: [Int] -> Bool )
    quickCheck (functorCompose :: IntFC)





-- 8
data Trivial = Trivial
-- has kind (*) so cannot be FUnctor, must have kind (* -> *)


