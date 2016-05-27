import Control.Applicative ((*>))
import Control.Monad (join)


{-
NOTE
class Applicative m => Monad m where

    -- note takes result from operatoin on left side and puts it as arg to
    the operation on the right side
    (>>=) :: m a -> (a -> m b) -> m b

    -- note sequences two operations, discards result of first action
    (>>) :: m a -> m b -> m b

    return :: a -> m a
-}

{-
NOTE
fmap f xs  ======= xs >>= return . f

-}

{-
NOTE

fmap  :: Functor f     =>   (a -> b) -> f a        -> f b
(<*>) :: Applicative f => f (a -> b) -> f a        -> f b
(>>=) :: Monad f       => f a        -> (a -> f b) -> f b
-}



{-
NOTE

join :: Monad m => m (m a) -> m a
concat :: [[a]] -> [a]
-}


{-
SUMMARY KEY EQUALS
SUMMARY KEY EQUALS
SUMMARY KEY EQUALS
--> 1. fmap a function over the contents that are held in a monadic structure.
--> 2. that function used to fmap returns a monadic structure. When we fmap that
function over the monadic structure we get two layers of monadic structure
around the content.
--> 3. after getting double layers, smush them - Join!
-}
-- ANSWER
bind :: Monad m => (a -> m b) -> m a -> m b
bind f xs = join $ fmap f xs





{-
NOTE: monad also lifts!

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftM :: Monad m =>  (a -> r) -> m a -> m r
liftM = same as fmap just with different typeclass constraint

example
fmap (+1) [1,2,3]
= [2,3,4]

liftA (+1) [1,2,3]
= [2,3,4]

liftM (+1) [1,2,3]
= [2,3,4]
-------------------------------------------------------------------------------

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c

example  they are the same

(,) <$> (Just 3) (Just 5)
= Just (3,5)

liftA2 (,) (Just 3) (Just 5)
= Just (3,5)

liftm2 (,) (Just 3) (Just 5)
= Just (3,5)

-------------------------------------------------------------------------------
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d

example they are the same

liftA3 (,,) [1,2] [3,4] [5,6]
= [(1,3,5),(1,3,6),(1,4,5),(1,4,6),(2,3,5),(2,3,6),(2,4,5),(2,4,6)]

liftM3 (,,) [1,2] [3,4] [5,6]
= [(1,3,5),(1,3,6),(1,4,5),(1,4,6),(2,3,5),(2,3,6),(2,4,5),(2,4,6)]
-}










-- 18.3 DO SYNTAX AND MONADS --------------------------------------------------

{-
NOTE These operators do the same thing.

(*>) :: Applicative f => f a -> f b -> f b

(>>) :: Monad m => m a -> m b -> m b

-}

sequencing :: IO()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"


-- desugared version 1
sequencing' :: IO()
sequencing' =
    putStrLn "blah" >>
    putStrLn "another thing"

-- desugared version 2
sequencing'' :: IO()
sequencing'' =
    putStrLn "blah" *>
    putStrLn "another thing"


-------------------------------------------------------------------------------------
binding :: IO()
binding = do
    putStr "Input: "
    name <- getLine
    putStrLn name

binding' :: IO()
binding' =
    getLine >>= putStrLn
    -- note: this means instead of naming the variable, we use >>=
    -- to pass it directly.

-------------------------------------------------------------------------------------

-- note: Monad is important for its join.
-- note: join merges the two IO effects
-- notes: goes from String -> IO -> IO(IO())
-- to: String -> IO() -- note: it merges the two IOs
q :: IO (IO ()) -- note: outer IO is for getLine, inner IO is for putStrLn
q = putStrLn <$> getLine
-- now
q' :: IO()
q' = join $ putStrLn <$> getLine -- will repeat your input on next line
q'' = join $ fmap putStrLn getLine -- will repeat your input on next line

-------------------------------------------------------------------------------------
-- note: desugar do syntax

bindingAndSequencing :: IO()
bindingAndSequencing = do
    putStrLn "name pls: "
    name <- getLine
    putStrLn ("y helo thar: " ++ name)


bindingAndSequencing' :: IO()
bindingAndSequencing' =
    putStrLn "name pls: " >>
    getLine >>= -- note: bind operator should be read as do f1 then f2 and arg is passed indirectly.
    \name -> putStrLn ("y helo thar: " ++ name)




twoBinds :: IO()
twoBinds = do
    putStrLn "name pls: "
    name <- getLine
    putStrLn "age pls: "
    age <- getLine
    putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")


twoBinds' :: IO()
twoBinds' =
    putStrLn "name pls: " >>
    getLine >>=
    \name ->                  -- note: need to do \name and \age separately since we
    putStrLn "age pls: " >>   -- must mark down the user input else it would be
    getLine >>=               -- covered by the next input.
    \age ->
    putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")



twoBinds'' :: IO()
twoBinds'' =
    putStrLn "name pls: " >>
    getLine >>=
        (\name ->                  -- note: need to do \name and \age separately since we
        putStrLn "age pls: " >>   -- must mark down the user input else it would be
        getLine >>=               -- covered by the next input.
            (\age ->
            putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")))



------------------------------------------------------------------------------------
-- 18.4 Examples of Monad use

-- LIST -----------------------------------------------------------------------------
{-
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>=) ::            [a] -> (a -> [b]) -> [b]

return :: Monad m => a -> m a
return ::            a -> [a]
-}
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x then [x^2, x^2] else [x^2]

-- note: the do hides the join of the List monad
twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    x <- xs
    if even x then [x^2, x^2] else []


d xs = [[x^2,x^2] | x <- xs, even x]






-- MAYBE ---------------------------------------------------------------------------
{-
note: m ~ Maybe
(>>=) :: Monad m => m a -> (a ->     m b) ->     m b
(>>=) ::        Maybe a -> (a -> Maybe b) -> Maybe b

return :: Monad m => a ->     m a
return ::            a -> Maybe a

instance Monad Maybe where
    return x = Just x
    (Just x) >>= k     = k x    key so run the entire computation over x
    Nothing >>=  _     Nothing  key so the entire computation is dropped
-}





-- note: Why we can't use this with Applicative:
-- if you return() something at the end of this type of pattern,
-- then use Applicative but if you just have a statement then that is going to
-- produce more monadic structure so you need the implicit monad join to crunch
-- it back down.

{-
NOTE SUMMARY :
1. a) With Maybe Applicative you can have a fail then success right after.
   The computation doesn't end because ofa fail - it goes on.
   b) Each Maybe computation fails or succeeds independently of each other. SO
   unlike in Monad, the arg isn't used to fuel the next action.

2. a) With Maybe Monad, computations halt when a fail is encountered.
   b) Computations are fuled by each other. So an arg is used to fuel the next
   computation.
-}
-- EXAMPLE

f :: Maybe Integer
f = Just 1
g :: Maybe String
g = Just "1"
h :: Maybe Integer
h = Just 10191

zed :: a -> b -> c -> (a, b, c)
zed = (,,)

doSomething = do
    a <- f
    b <- g
    c <- h
    return (zed a b c)

zedM :: Monad m => a -> b -> c -> m (a, b, c)
zedM a b c = return (a, b, c)

doSomethingM = do
    a <- f
    b <- g
    c <- h
    zedM a b c

-- help: how does this relate to above text? how does it get crunched?
-- HELP also how is one fit for applicative and the other fit for monad?




-- EITHER --------------------------------------------------------------------------

{-
note: m ~ Either e
(>>=) :: Monad m => m a -> (a ->        m b) ->        m b
(>>=) ::     Either e a -> (a -> Either e b) -> Either e b

return :: Monad m => a ->        m a
return ::            a -> Either e a

-}
-- years ago
type Founded = Int
-- number of programmers
type Coders = Int

data SoftwareShop =
    Shop {
        founded     :: Founded,
        programmers :: Coders
    } deriving (Eq, Show)

data FoundedError = NegativeYears Founded
                  | TooManyYears Founded
                  | NegativeCoders Coders
                  | TooManyCoders Coders
                  | TooManyCodersForYears Founded Coders
                  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
    | n < 0     = Left $ NegativeYears n
    | n > 500   = Left $ TooManyYears n
    | otherwise = Right n


validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0     = Left $ NegativeCoders n
    | n > 5000  = Left $ TooManyCoders n
    | otherwise = Right n


mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded     <- validateFounded years
    programmers <- validateCoders coders
    if programmers > founded `div` 10
        then Left $ TooManyCodersForYears founded programmers
        else Right $ Shop founded programmers








{-
NOTE monad instance of apply operator (<*>) must quit the moment a failure
pops up since arguments are fed into the next operation, which can't continue
if there's a failure.
For applicative, things happen independently and are not fed into continuations.

note

(<*>) :: Applicative f => f (a -> b) -> f a -> f b
ap    ::      Monad  m => m (a -> b) -> m a -> m b


HELP understand what point this is delivering:
ap :: (Monad m) => m (a -> b) m a -> m b
ap m m' = do
    x <- m
    x' <- m'
    return (x x')

-}





{-uncover
main = do
    print $ d [1..10]
    print $ join $ d [1..10]
    (print . join . d) [1..10]
    putStrLn ""
    --------------------------------------
    print $ mkSoftware 0 0
    print $ mkSoftware (-1) 0
    print $ mkSoftware (-1) (-1)
    print $ mkSoftware 0 (-1)
    print $ mkSoftware 500 0
    print $ mkSoftware 501 0
    print $ mkSoftware 501 501
    print $ mkSoftware 100 5001
    print $ mkSoftware 0 500
-}














-- 18.5 MONAD LAWS -----------------------------------------------------------

{-
NOTE

(>>=) :: Monad m => m a -> (a -> m b) -> m b
--                 [1]        [2]        [3]

1. Identity laws:

    right identity:
    m >>= return = m
   [1]      [2]   [3]

    left identity:
    return x >>= f = f x
       [1]      [2]  [3]



2. Associativity laws:


-}