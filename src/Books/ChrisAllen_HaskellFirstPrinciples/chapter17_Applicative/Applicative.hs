import Control.Applicative
import Data.Monoid -- for Sum


{-
NOTE
Applicative is a monoidal functor. Applies a functor over structures by smashing
them together.

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

note
-- fmap
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b


note
Along with these core functions, the Control.Applicative library provides
some other convenient functions: liftA, liftA2, and liftA3:

liftA :: Applicative f =>
    (a -> b)
    -> f a
    -> f b
liftA2 :: Applicative f =>
    (a -> b -> c)
    -> f a
    -> f b
    -> f c
liftA3 :: Applicative f =>
    (a -> b -> c -> d)
    -> f a
    -> f b
    -> f c
    -> f d


    note liftA is basically fmap only with an Applicative typeclass constraint
    instead of a Functor one. But all Applicative instances are also functors.

-}




-- 17.3 FUNCTOR vs. APPLICATIVE -----------------------------------------------------

-- LAW: fmap f x = pure f <*> x
{-uncover
main = do
    print $ fmap (+1) [1,2,3]
    print $ pure (+1) <*> [1..3]
-}


{- NOTE
mappend :: Monoid a => a -> a -> a

So, with Applicative, we have a Monoid for our structure and function
application for our values!

mappend :: f f f
$ :: (a -> b) a b

(<*>) :: f (a -> b) -> f a -> f b
-- plus Functor fmap to be able to map
-- over the f to begin with.


So in a sense, we’re bolting a Monoid onto a Functor to be able to deal
with functions embedded in additional structure. In another sense,
we’re enriching function application with the very structure we were
previously merely mapping over with Functor.
-}

-- EXAMPLES
{-uncover
main = do
    -- note So here in f (a -> b) the list is (f). THe mappend of applicative
    -- prevents the final result from being a list of lists. It joins since it
    -- is a monoidal functor.
    print $ [(*2), (*3)] <*> [4,5]
    -- note The f is the Maybe. The join aspect prevents it from being a maybe
    -- of maybe.
    print $ (Just (*2)) <*> Just 2
    print $ Just (*2) <*> Nothing
    --print $ Nothing <*> Just 2  -- HELP error when printing!
    --print $ Nothing <*> Nothing -- HELP error when printing!
-}



{-
NOTE

Prelude> :info (,)
data (,) a b = (,) a b -- Defined in ‘GHC.Tuple’
...
instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
...
instance (Monoid a, Monoid b) => Monoid (a, b)

For two tuples, the 'a' part must be Monoid type since the 'a's in the two
corresponding tuples must be joined. Not needed for the 'b' part since the
functor is just applied over that. (help - the functor is applied or what?)
-}

-- Example
{-uncover
main = do
    print $ ("Woo", (+1)) <*> (" Hoo!", 0)
    print $ ((Sum 2), (+1)) <*> ((Sum 0), 0)
    print $ ((Product 3), (+9)) <*> ((Product 2), 8)
    print $ ((All True), (+1)) <*> ((All False), 0)
-}










{-
NOTE TUPLE MONOID AND APPLICATIVE --------------------------------------------------

-- HELP what does a tuple of mempty mean?

instance (Monoid a, Monoid b) => Monoid (a, b) where
    mempty = (mempty, mempty)
    (a, b) 'mappend' (a', b') = (a 'mappend' a', b 'mappend b')


HELP what does the tuple in the first part of the tuple mean?

instance Monoid a => Applicative ((,), a) where
    pure x = (mempty, x)
    (u, f) <*> (v, x) = (u `mappend` v, f x)






NOTE MAYBE MONOID and APPLICATIVE ------------------------------------------------

note Monoid has kind (*) so just like Functor, use the argument to reduce kind
of the type for which we are writing the instance.


instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    mappend m Nothing = m
    mappend Nothing m = m
    mappend (Just a) (Just a') = Just (mappend a


HELP Applicative has kind (* -> *) which is a function so how do we write
the type instance?
HELP why is there no monoid mention in the applicative?

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    Just f <*> Just a = Just (f a)


HELP understand this paragraph: pg 657:

While applicatives are really monoidal functors, be careful about
taking it too literally. For one thing, Monoid and Applicative instances
aren’t required or guaranteed to have the same monoid of structure,
and the functorial part may actually change things. Nevertheless,
you might be able to see the implicit monoid in how the Applicative
pattern matches on the Just and Nothing cases and compare that with
this Monoid:
-}










-- 17.5 APPLICATIVE IN USE -------------------------------------------------------


{-

LIST APPLICATIVE --------------------------------------------------------------------

-- f ~ []
(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: [ ] (a -> b) -> [ ] a -> [ ] b

-- more syntactically typical
(<*>) :: [(a -> b)] -> [a] -> [b]

-- equals means a function containing functions mapped over content to return
-- changed content wrapped in the outside function.

pure :: a -> f a
pure :: a -> [ ] a


example list
[(+1), (*2)] <*> [2,4]
= [ (+1) 2 , (+1) 4 , (*2) 2 , (*2) 4 ]
= [2, 5, 4, 8]

so here the (a -> b) functions are encased in a list.

note
The fact that it doesn’t
return two lists or a nested list or some other conﬁguration in which
both structures are preserved is the monoidal part; the reason we
don’t have a list of functions merely concatenated with a list of values
is the function application part



example tuple
= (,) <$> [1,2] <*> [3,4]
 = [(1, ), (2, )] <*> [3,4]
 = [(1,3), (1,4), (2,3), (2,4)]

note could write instead
= liftA2 (,) [1,2] [3,4]
= [(1,3),(1,4),(2,3),(2,4)]




example
= (+) <$> [1,2] <*> [3,5]
= [4,6,5,7]

= liftAd (+) [1,2] [3,5]
= [4,6,5,7]




example
= max <$> [1,2] <*> [1,4]
= [1,4,2,4]

= liftA2 max [1,2] [1,4]
= [1,4,2,4]
-}



-- example

f x = lookup x [(3, "hello"), (4, "julie"), (5, "jbai")]
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2,3), (5,6), (7,8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

{-uncover
main = do
    print $ f 3
    print $ g 8
    print $ (++ ) <$> f 3 <*> g 7 -- HELP why can't add space to attachment symbol?
    print $ (+) <$> h 5 <*> m 1
    print $ (+) <$> h 5 <*> m 6
    ----------------------------------------- with liftA2
    print $ liftA2 (++) (g 9) (f 4) -- note fmap length $ liftA2 (++) (g 9) (f 4)
    print $ liftA2 (^) (h 5) (m 4)
    print $ liftA2 (*) (h 5) (m 4)
    print $ liftA2 (*) (h 1) (m 1)
    ----------------------------------------- IO
    putStrLn "Get two for (++) "
    (++) <$> getLine <*> getLine
    putStrLn "Get two for (,) "
    (,) <$> getLine <*> getLine
-}











-- IDENTITY -------------------------------------------------------------------------

{- NOTE
-- f ~ Identity
-- Applicative f =>
(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: Identity (a -> b) -> Identity a -> Identity b

pure :: a -> f a
pure :: a -> Identity a
-}

-- Identity Exercise 17.5
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure x = Identity x -- note or could just write pure = Identity
    (Identity f) <*> (Identity a) = Identity (f a)

{-uncover
main = do
    print $ const <$> [1,2,3] <*> [9,9]
    print $ const <$> Identity [1,2,3] <*> Identity [9,9]
-}


{-
NOTE translation:

  const <$> [1,2,3] <*> [9,9,9,9]
= [const 1 9, const 1 9, const 1 9, const 1 9, const 2 9 ...] <*> [9,9,9,9]
= [1,1,1,1,  2,2,2,2,  3,3,3,3]

help is this what it looks like? Or are there lists of lists:
[[const 1 9, const 1 9, const 1 9, const 1 9], [const 2 9...]...]?
-}








-- CONSTANT ----------------------------------------------------------------------

{-
NOTE

-- f ~ Constant e

(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: Constant e (a -> b) -> Constant e a -> Constant e b

pure :: a -> f a
pure :: a -> Constant e a
-}


newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    -- note definition of Constant - throwing away function application
    fmap f (Constant b) = Constant b


instance Monoid a => Applicative (Constant a) where
    --pure x = Constant x -- HELP why wrong?
    pure _ = Constant mempty
    (Constant x) <*> (Constant y) = Constant (x <> y)
    -- HELP where is function aplication being thrown away? Why not like
    -- the Identity instance?


{-uncover
main = do
    print $ Constant (Sum 1) <*> Constant (Sum 2)
    --print $ Constant undefined <*> Constant (Sum 2)
    --print $ Constant (Sum 2) <*> Constant undefined
    print $ (pure 1 :: Constant String Int)
-}

    {-
    NOTE HELP figure out this error
    pure 1 :: Constant Int String
    pure 1 :: Constant Int Int
    -}













-- MAYBE APPLICATIVE --------------------------------------------------------------

{-
NOTE
-- f ~ Maybe

(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b

pure :: a -> f a
pure :: a -> Maybe a
-}


validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
    if (length s) > maxLen
    then Nothing
    else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

{-
NOTE
(a -> b) -> f a -> f b
:t Name :: (String -> Name)
:t Just "babe" :: Maybe String

(a -> b) -> f a -> f b
(String -> Name) -> Maybe String -> Maybe Name
-}
mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a


data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a
{-mkPerson n a =
    case mkName n of
        Nothing -> Nothing
        Just n' ->
            case mkAddress a of
                Nothing -> Nothing
                Just a' -> Just $ Person n' a'
-}

{-
NOTE EXPLANATION

Person <$> Just (Name "babe") <*> Just (Address "farm")

:t Person :: Name -> Address -> Person
:t (Just (Name "babe")) :: Maybe Name

(a -> b) -> f a -> f b

(Name -> (Address -> Person)) -> Maybe Name -> Maybe (Address -> Person)
   a  ->           b               f      a ->      f       b


note the fmap part: Person <$> mkName

fmap    f   (Just      a)        = Just (f         a)
fmap Person (Just (Name "babe")) = Just (Person (Name "babe"))
f ~ Person
a ~ Name "babe"


note the applicative part: <*> Just (Address "farm")

Person (Name "babe") is still awaiting the address function argument, so it is a
partially applied function.

:t Just (Person (Name "babe")) :: Maybe (Address -> Person)
:t Just (Address "farm") :: Maybe Address

-- We want to apply the partiall applied Person "babe" inside the Just to the farm
inside the Just

f (a -> b) -> f a -> f b

Maybe (Address -> Person) -> Maybe Address -> Maybe Person
f     (   a    ->    b  ) ->   f     a     ->   f     b
 f ~ Maybe
 a ~ Address
 b ~ Person



THis is the applicative maybe instance case we use:

Just f <*> Just a = Just (f a)

= Just (Person (Name "babe")) <*> Just  (Address "farm")
= Just (Person (Name "babe") (Address "farm"))
-} -- note look at cow example















-- 17.6 APPLICATIVE LAWS ----------------------------------------------------------

u = Just ((*8))
v = Just ((+3))
w = Just 10
{-uncover
main = do
    -- note IDENTITY LAW: pure id <*> v = v
    print $ pure id <*> [1..5] -- same as:
    print $ id <$> [1..5]
    print $ pure id <*> Just "Hello Applicative"
    --print $ pure id <*> Nothing -- HELP find out why throws error
    --print $ pure id <*> Left "Error'ish"
    --print $ pure id <*> Right 8001

    -- note COMPOSITION LAW: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    print $ pure (.) <*> u <*> v <*> w
    print $ (.) <$> u <*> v <*> w

    -- note HOMOMORPHISM LAW: structure preservation
    -- pure f <*> pure x
    print $ pure (+1) <*> (pure 1 :: Maybe Int)
    print $ pure (+1) <*> (pure 1 :: [Int])
    --print $ pure (+1) <*> (pure 1 :: (Either a Int)) -- HElp why error?

    -- note INTERCHANGE LAW: u <*> pure y = pure ($y) <*> u
    print $ [(+1), (*3)] <*> pure 1
    print $ pure ($ 1) <*> [(+1), (*3)]
    print $ Just (+3) <*> pure 1
    print $ pure ($ 1) <*> Just (+3)
-}
    {-
    NOTE ABOUT INTERCHANGE LAW:
    u = always a structure containing a function
    y = just a simple arg

    By section the $ function with the y we create an environment where y is there
    awaiting a function to apply to it:

    pure ($    y) <*>  u
      f  (a -> b)     f a
    pure ($    2) <*>  Just (+2)
    = Just ((+2) $ 2)
       f     a  -> b ->
    = Just 4

    -}













-- 17.8 ZIPLIST MONOID -------------------------------------------------------------

{- NOTE
[1, 2, 3] <> [4, 5, 6]
-- changes to
[
1 <> 4
, 2 <> 5
, 3 <> 6
]
-}

-- ZERO vs IDENTITY ------------------------------------------------------------------
-- HELP how to get identity for ZipList?
-- Sum 1 `mappend` ??? -> Sum 1