import Control.Applicative
import Control.Monad



newtype Reader r a = Reader {runReader :: r -> a}


--- 1
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f gx gy = f <$> gx <*> gy



--- 2
-- exercise 22.5 ask:
ask :: Reader a a
ask = Reader $ \x -> x -- equals Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader $ \x -> f x



--- 3
-- NOTE key important: we write Reader r because the constructor has only one argument
-- which is the function (r). So Reader rab means it has function of type r -> a -> b.

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ (f . ra)


instance Applicative (Reader r) where
 -- pure :: a -> Reader r a
    pure a = Reader $ \r -> a
 -- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
 -- (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)
    -- equals answer:


instance Monad (Reader r) where
    return x = Reader (\_ -> x) -- any function Reader gets should still result in x.
 -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> (runReader (aRb (ra r))) r
    {-
    key note process:
    1. (ra r) to get result of type (a)
    2. apply aRb to a ==> aRb (ra r)
    3. result is Reader r b so runReader to be able to pass in r again:
       runReader (aRb (ra r)) r
    4. then wrap it in Reader: Reader $ ...
    -}


{-
instead of Reader you could just declare:

Okay, so the reader monad is just a function. Why have Reader at all? Good question.
Actually, you don't need it!

instance Functor ((->) env) where
   fmap = (.)

 instance Monad ((->) env) where
   return = const
   f >>= g = \x -> g (f x) x

-}




--- 4

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person { humanName :: HumanName,
                       dogName :: DogName,
                       address :: Address} deriving (Eq, Show)

data Dog = Dog {dogsName :: DogName, dogsAddress :: Address} deriving (Eq, Show)

---
pers :: Person
pers = Person (HumanName "Sasha") (DogName "Barker") (Address "Forest Grove")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")
---

getDogR :: Reader Person Dog
getDogR = Reader $ liftA2 Dog dogName address




--- Implementing with Reader Monad.
-- help why called Reader Monad if it doesn't use Reader?
getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addr <- address
    return (Dog name addr)


--- Using the actual Reader Monad
getDogRM' :: Reader Person Dog
getDogRM' = Reader $ liftM2 Dog dogName address




main :: IO()
main = do
    print $ (runReader getDogR) pers
    print $ (runReader ask) "whatever you wrote"
    -- print $ (runReader asks)- help how to run this?
    print $ getDogRM pers
    print $ getDogRM chris
    print $ (runReader getDogRM' pers)
    -- note: runReader to unwrap Reader then pass in person to get Dog. 