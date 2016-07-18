import Control.Applicative
-- import Control.Monad.Reader



hurr = (*2)
durr = (+10)

m :: Integer -> Integer
m = hurr . durr


-- note called the functor of functions
m1 :: Integer -> Integer
m1 = fmap hurr durr -- equals (*2) ((+10) x)


m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr
-- equals ((+) <$> (3*2) <*> (3+10)) => ((3 * 2) + (3 + 10))

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr -- same as above.


{-
1) NOTE
composition yields same result as fmapping:
--- > fmap hurr durr 1 == 22
--- > (hurr . durr) 1 == 22

example
((+) . (*2)) 5 3
13
example
((+) <$> (*2)) 5 3
13

key meaning
((+) . (*2)) == \x -> (+) (2 * x)



2) NOTE:
((+) . (*2)) 5 3
= (\x -> (+) (2 * x)) 5 3
= (\5 -> (+) (2 * 5)) 3
= ((+) 10) 3
= 13


todo: what is the difference between applicative apply and applicative __ ? ask tati.

3) NOTE
((+) <$> (*2) <*> (+10)) 3
= ((+) <$> (3*2) <*> (3+10))
= ((3 * 2) + (3 + 10))



-}


-- NOTE: intuition for Reader: Reader is a way to string functions together when
-- they are waiting for input from shared environment.
-- Using Reader avoids passing that argument around explicity.

-- example
(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) f g x = ((||) <$> f <*> g) x
-- or note:
-- (<||>) = liftA2 (||)



-- example
-- help help help understand why monadic context is used?
hurrDurr :: Integer -> Integer
hurrDurr = do
    a <- hurr -- argument gets applied to hurr resulting in integer (a)
    b <- durr -- arg applied to durr, resulting in integer (a).
    return (a + b) -- integers are added.







--- 22.4 FUNCTOR OF FUNCTIONS ------------------------------------------------------------
{-
KEY COMPARE


-- Note there is an instance of (->) for Functor:

instance Functor ((->) r) where
    fmap = (.)

    note equals: ((->) r) is (r -> ) so r is the type of the argument to the function.
    Or can be written: (a -> b) where (r) is (a).
    So (r), the argument for functions, is part of the structure being lifted over,
    not the value being transformed.

    (.)  ::              (b -> c) -> (a -> b) -> a -> c
    fmap :: Functor f => (a -> b) -> f a -> f b
    equals
    :: (b -> c) -> (a -> b) -> a -> c
    :: (a -> b) -> f a -> f b
    equals
    :: (b -> c) -> (a -> b) -> a -> c
    :: (b -> c) -> f b -> f c
    equals (because f is ((->) a)
    :: (b -> c) -> (a -> b) -> a -> c
    :: (b -> c) -> ((->) a) b -> ((->) a) c
    equals
    :: (b -> c) -> (a -> b) -> a -> c
    :: (b -> c) -> (a -> b) -> a -> c


example
fmap (+1) (*2) 3
= fmap (+1) (*2) $ 3
= (fmap (+1) (*2)) 3
= (+1) . (*2) $ 3
= (+1) <$> (*2) $ 3
-}









--- 22.5 READER ------------------------------------------------------------------------

-- Note reader is the newtype wrapper for the function type ((->) r)

{-
newtype Reader r a = Reader {runReader :: r -> a}

--- > purpose:  It should accept a value of some type e (for environment) that
-- represents the data that we're passing in, and return a value of some other
-- type a as its result.
--- > r is the type we are reading in
--- > a is the result type of the function.


NOTE functor instance

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader $ \r -> f (ra r)
    equals
    fmap f (Reader ra) = Reader $ (f . ra)

equals
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g = \x -> f (g x)

equals
\r -> f (ra r)
\x -> f (g x)


important note: in words, we are basically doing this:
1. unpack r -> a out of Reader
2. Compose f with the function we unpacked out of Reader (which is ra)
3. put the new function made from the composition back into Reader.

-}






--- 22.6. APPLICATIVE OF FUNCTIONS ------------------------------------------------------

{-
KEY COMPARE
pure :: a -> f a
pure :: a -> (r -> a)

(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)

-}


newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person { humanName :: HumanName,
                       dogName :: DogName,
                       address :: Address} deriving (Eq, Show)

data Dog = Dog {dogsName :: DogName, dogsAddress :: Address} deriving (Eq, Show)


pers :: Person
pers = Person (HumanName "Sasha") (DogName "Barker") (Address "Forest Grove")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address
{-
equals as it was here:

((+) <$> (*2) <*> (+10)) 3
= ((+) <$> (3*2) <*> (3+10))
= ((3 * 2) + (3 + 10))

equals evaluation:
(Dog <$> dogName <*> address) p
= (Dog <$> dogName <*> address) (Person (HN "Sasha") (DN "Barker") (AD "Forest Grove"))
= (Dog <$> "Barker" <*> "Forest Grove")
= Dog (DogName "Barker") (Address "Forest Grove")

-}









--- 22.7 MONAD OF FUNCTIONS ------------------------------------------------------------

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

-- note increments values inside the structure and tells us length of the value.
froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

-- note abstracting frooty
-- note equals bind monad type: ---> now this is the monad of functions ... help
-- (>>=) :: Monad m =>   m a   -> (a -> (m b))    ->   m b
-- fooBind ::         (r -> a) -> (a -> (r -> b)) -> (r -> b)
-- equals the (-> r) is equivalent to the Monad (m) in the type of (>>=)
fooBind :: (r -> a) -> (a -> r -> b) -> r -> b
fooBind m k = \r -> k (m r) r





--- The Monad Instance ------------------
-- KEY COMPARE
-- (>>=) :: Monad m =>   m a -> (a -> m b)      ->   m b
-- (>>=) ::        (->) r a  -> (a -> (->) r b) -> (->) r b
-- (>>=) ::         (r -> a) -> (a -> r -> b)   -> r -> b

-- return :: Monad m => a ->      m a
-- return ::            a -> (->) r a
-- return ::            a ->   r -> a

-- NOTE comparing bind with applicative:
-- (<*>) :: (->) r (a -> b) -> (->) r a        -> (->) r b
-- (>>=) :: (->) r a        -> (a -> (->) r b) -> (->) r b

