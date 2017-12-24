
import Data.Char
import Control.Monad (liftM, ap)
import Control.Applicative

-- Functor --------------------------------------------------------------------------
--  uses type constructor that let you apply functions to their content.
--
-- class Functor fr where
--     fmap :: (a -> b) -> frb a -> fr b
{-
fr is the type constructor
fmap takes function that transforms a into b and applies it to the type fr a
to result in fr b.
fmap reaches inside f a to transform its contents.
Ex: regular list constructor [a] is a Functor

Ex:
instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)  --note: f gets under the skin of Maybe.

Ex:
instance Functor (State s) where
    fmap f act = state $ \st ->
        let (x, st') = runState act st
        in (f x, st')


note: List Bind
To bind two non-deterministic functions, apply the second one to each element of
the list returned by the first and then collapse results into single list using join.

xs >>= k = join (fmap k xs)
-}



data List a = Nil | Cons a (List a)
--    deriving Show


instance (Show a) => Show (List a) where
    show Nil = ""
    show (Cons x xs) = show x ++ ", " ++ show xs


instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)


instance Monad List where
    return x = Cons x Nil
    xs >>= k = join $ fmap k xs
    -- note: general form: ma >>= k = join $ fmap k ma
    -- note: means starting with xs apply continuation to them.

instance Applicative List where  -- help
    pure = return
    (<*>) = ap



join :: List (List a) -> List a  -- note: m(m a) -> m a
join Nil = Nil
join (Cons xs xss) = cat xs (join xss)
--join mma = mma >>= id
-- note: start with doubly wrapped arg, then bind extracts
-- a singly wrapped center so then pick id as continuation.
-- help: but how's this definition used? It doesn't compile.



cat :: List a -> List a -> List a
cat Nil ys = ys
cat (Cons x xs) ys = Cons x (cat xs ys)


-- output 1
l1 = Cons 1(Cons 2 Nil)
l2 = Cons 3 Nil


-- output 2
neighbors :: (Num a) => a -> a -> List a
neighbors x dx = Cons (x-dx) (Cons x (Cons (x+dx) Nil))

test = do
    x <- neighbors 0 100
    y <- neighbors x 1
    return y

{-
main = do
    --print $ join $ Cons l1 (Cons l2 Nil)
    print $ test
-}





-- Exercise 1 ------------------------------------------------------------------
-- Define join for Maybe

joinM :: Maybe (Maybe a) -> Maybe a
joinM Nothing = Nothing
joinM (Just mbe) = mbe

test1, test2, test3 :: Maybe (Maybe String)
test1 = Nothing
test2 = Just Nothing
test3 = Just (Just "a little something")

{-
main = do
    print $ joinM test1
    print $ joinM test2
    print $ joinM test3

-}


-- Exercise 2 --------------------------------------------------------------------
-- define listBind and listReturn

listBind :: [a] -> (a -> [b]) -> [b]
listBind xs k = concat (map k xs)

listReturn :: a -> [a]
listReturn x = [x]


neighbors2 x = [x - 1, x, x + 1]
{-
main = do
    print $ listBind [10, 20, 30] neighbors2
    print $ listBind "string" (listReturn . ord)

-}













-- Composition of monadic functions and Kleisli -----------------------------------
{-
note: monadic functions can be composed using fish operator (Kleisli operator):

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
g <=< f = \x -> f x >>= g

x is of type 'a' and f is applied to x which gives type m b.
THen we apply g to it.
-}



(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
g <=< f = \x -> f x >>= g

f x = [x, x + 1]
g x = [x * x]

test4 = g <=< f

--main = print $ test4 7
-- help: how does 'g' know to map over the list given by 'f'?



-- Exercise 3 ---------------------------------------------------------------------
-- implement the other fish operator that composes from left to right
{-
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g
-- note: x is still type 'a' but now this is implemented the other way around.

test5 = f >=> g
-}
--main = print $ test5 7


-- Exercise 4 ---------------------------------------------------------------------
-- expression fish operator for standard lists considering the non-deterministic
-- function interpretation

(>=>) :: (a -> [b]) -> (b -> [c]) -> (a -> [c])
f >=> g = \x -> concat (map g (f x))

modCase c = [toLower c, toUpper c]
camelize = modCase >=> modCase

{-
main = do
    print $ fmap camelize "Hump"
    print $ fmap camelize "Gregory"
    print $ fmap modCase "Hump"
-}
------------------------------------------------------------------------------------
-- List Monad and List COmprehension

squares lst = do
    x <- lst           -- note: means drawing an element x from lst help ???
    return (x*x)

--main = print $ squares [1,2,3]

{-
note: desugared internally: (since instance Monad exists for built-in lists)
squares lst = lst >>= \x -> return (x*x)

expanding >>= and return help
squares lst =
    concat $ fmap k lst
    where
        k = \x -> [x*x]

basic idea: here fmap k produces a list of one-elemtn lists of squares.
Then squashed by concat.
-}




-- Exercise 5 ---------------------------------------------------------------------
-- Rank (Ace - King)
-- Suit (Club, Diamond, Heart, Spade)
-- write a list comprehension that generates all cards in a deck.

data Suit = Club | Diamond | Heart | Spade
    deriving (Show, Enum) -- what does deriving enum mean? help

data Rank = Rank Int

instance Show Rank where
    show (Rank 1) = "Ace"
    show (Rank 11) = "Jack"
    show (Rank 12) = "Queen"
    show (Rank 13) = "King"
    show (Rank i) = show i

deck = [(Rank r, s) | s <- [Club .. Spade],
                      r <- [1 .. 13]]

--main = print deck

-- Exercise 6 ----------------------------------------------------------------------
-- quick sort

q [] = []
q (p:xs) = q [x | x <- xs, x < p]
        ++   [p]
        ++ q [x | x <- xs, x >= p]


main = print $ q [2,5,1,3,4]