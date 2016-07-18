import Control.Applicative



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
-- help help help help todo todo todo understand these better...
-- help key note question: why do we write Reader rab? WHy not Reader r ab?

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ (f . ra)


instance Applicative (Reader r) where
 -- pure :: a -> Reader r a
    pure a = Reader $ \r -> a
 -- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
 -- (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)


instance Monad (Reader r) where
    return x = Reader (\_ -> x)
 -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r
    -- (Reader f) >>= g = Reader $ \x -> runReader (g (f x)) x




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




--- Implementing with Reader Monad
getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addr <- address
    return (Dog name addr)


main :: IO()
main = do
    print $ (runReader getDogR) pers
    print $ (runReader ask) "whatever you wrote"
    -- print $ (runReader asks)- help how to run this?
    print $ getDogRM pers
    print $ getDogRM chris