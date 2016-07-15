
import Data.Foldable
import Data.Monoid
import Data.Functor.Identity
import Data.Functor.Constant
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{-
Traversable: used to traverse data structure, mapping a function inside a structure
while accumulating the applicative contexts along the way.

class (Functor t, Foldable t) => Traversable t where

    -- | Map each element of a structure to an action,
    -- evaluate these actions from left to right, and
    -- collect the results. For a version that ignores
    -- the resuls see 'Data.Foldable.traverse_'.
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f = sequenceA . fmap f

    sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id

-}

{-
NOTE
key brief version ------------------------------------------------------------------------

traverse  :: (a -> f b) -> t a -> f (t b)       (Applicative f)
mapM      :: (a -> m b) -> t a -> m (t b)       (Monad m)
sequenceA :: t (f a)    -> f (t a)
sequence  :: t (m a)    -> m (t a)

traverse_  :: (a -> f b) -> t a -> f ()
mapM_      :: (a -> m b) -> t a -> m ()
sequence_  :: t (m a)    -> m ()
sequenceA_ :: t (f a)    -> f ()


key longer version ------------------------------------------------------------------------

traverse :: (Applicative f, Traversable t)  => (a -> f b) -> t a -> f (t b)
mapM :: (Monad m, Traversable t)            => (a -> m b) -> t a -> m (t b)
sequenceA :: (Applicative f, Traversable t) => t (f a)    -> f (t a)
sequence :: (Monad m, Traversable t)        => t (m a)    -> m (t a)

traverse_ :: (Applicative f, Foldable t)  => (a -> f b) -> t a -> f ()
mapM_ :: (Monad m, Foldable t)            => (a -> m b) -> t a -> m ()
sequenceA_ :: (Applicative f, Foldable t) => t (f a)    -> f ()
sequence_ :: (Monad m, Foldable t)        => t (m a)    -> m ()
-}


--- 21.3 sequenceA ------------------------------------------------------------------------

e1 = fmap sum [Just 1, Just 2, Just 3]
e2 = fmap product [Just 1, Just 2, Just 3]
e3 = fmap product [Just 1, Just 2, Nothing]
e4 = (fmap . fmap) sum Just [1, 2, 3]
e5 = (fmap . fmap) product Just [1,2,3]

e6 = fmap Just [1, 2, 3]
s1 = sequenceA $ fmap Just [1,2, 3]
s2 = sequenceA [Just 1, Just 2, Just 3]
s3 = sequenceA [Just 1, Just 2, Nothing]
s4 = fmap sum $ sequenceA [Just 1, Just 2, Just 3]
s5 = fmap product $ sequenceA [Just 3, Just 4, Nothing]
s6 = sequenceA (Right (Just [1,2])) -- note same as Right <$> (Just [1,2])

s1_ = sequence_ (Just [1,2,3])
s2_ = sequence_ [Just 1, Just 2, Just 3]


--- 21.4 traverse ------------------------------------------------------------------------


-- traverse :: (a -> f b) -> t a -> f (t b)
-- fmap     :: (a -> b)   -> f a -> f b
-- (=<<)    :: (a -> m b) -> m a -> m b

t1 = sequenceA $ fmap Just [1,2,3]
t2 = sequenceA . fmap Just $ [1,2,3]
t3 = traverse Just [1,2,3]


-- :t ((sequenceA .) . fmap)
-- ((sequenceA .) . fmap)
-- :: (Applicative f, Traversable t) => (a1 -> f a) -> t a1 -> f (t a)
-- note (sequence .) . fmap NOT sequence . fmap because sequence must get applied
-- to result of fmap:
-- note (sequence .) . fmap = \f xs = sequence (fmap f xs)









--- 21.7 AXING TEDIUOUS CODE ------------------------------------------------------------

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO[String]
fetchFn = undefined

-- context initializer that has IO side effects
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

{- note before
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
    array <- fetchFn query
    case sequence (map decodeFn array) of
        (Left err) -> return $ Left $ err
        (Right res) -> do
            pairs <- makeIoOnlyObj res
            return $ Right pairs
-}

-- traverse  :: (a -> f b) -> t a -> f (t b)       (Applicative f, Traversable t)
-- mapM      :: (a -> m b) -> t a -> m (t b)       (Monad m, Traversable t)
-- (=<<)     :: (a -> m b) -> m a -> m b           (Monad m)

-- note after
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
    array <- fetchFn query
    traverse makeIoOnlyObj (mapM decodeFn array)

-- note even better
pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' = (traverse makeIoOnlyObj . mapM decodeFn =<<) . fetchFn

-- note or better still
-- help help
pipelineFn'' = (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn







------------------------------------------------------------------------------------------
-- traverse  :: (a -> f b) -> t a -> f (t b)
ex1 = traverse (Identity . (+1)) [1, 2]
ex2 = runIdentity ex1
edgelordMap f t = runIdentity $ traverse (Identity . f) t
ex3 = edgelordMap (+1) [1..5]

xs = [1, 2, 3, 4, 5] :: [Sum Integer]
ex4 = traverse (Constant . (+1)) xs
foldMap' f t = getConstant $ traverse (Constant . f) t
ex5 = foldMap' (+1) xs

-- note reduces to monoid under traversable
-- foldMap' :: (Traversable t, Monoid a) => (a1 -> a) -> t a1 -> a
-- note reduces to monoid under foldable
-- foldMap  :: (Foldable t, Monoid m)    => (a -> m ) -> t a  -> m



--- 21.9 TRAVERSABLE INSTANCES ---------------------------------------------------------

-- explaining Either type
data Choice a b = Wrong a | Correct b deriving (Eq, Show) ------------------------------


instance Functor (Choice a) where
    fmap _ (Wrong x)   = Wrong x
    fmap f (Correct y) = Correct (f y)

instance Applicative (Choice e) where
    pure a          = Correct a
    Wrong b <*> _   = Wrong b
    Correct f <*> r = fmap f r

instance Foldable (Choice a) where
    foldMap _ (Wrong _)   = mempty
    foldMap f (Correct y) = f y

    foldr _ z (Wrong _)   = z
    foldr f z (Correct y) = f y z

    foldl _ z (Wrong _)   = z
    foldl f z (Correct y) = f z y

instance Traversable (Choice a) where
    traverse _ (Wrong x)   = pure (Wrong x)
    traverse f (Correct y) = Correct <$> f y



{-
instance Functor ((,) a) where
    fmap f (x,y) = (x, f y)

instance Monoid a => Applicative ((,) a) where
    pure x = (mempty, x)
    (u, f) <*> (v, x) = (u <> v, f x)

instance Foldable ((,) a) where
    foldMap f (_, y) = f y
    foldr f z (_, y) = f y z
    foldl f z (_, y) = f z y


example
*Main> fmap (foldMap Sum) ([1,2,3], [4,5,6])
([1,2,3],Sum {getSum = 15})


-}






--- 21.10 TRAVERSABLE LAWS ---------------------------------------------------------------

{-
NOTE Laws for traverse

1) Naturality
t . traverse f = traverse (t . f)


2) Identity
traverse Identity == Identity

idea travsing Identity constructor over a value will make same value as just
putting the value in Identity
equals which means the traversable instance cannot add or inject any structure or
"effects"


3) Composition
traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f

idea we can collapse sequential traversal into single traversal by using Compose
which combines structure.




NOTE Laws for sequenceA

1) Naturality
t . sequenceA = sequenceA . fmap t


2) Identity
sequenceA . fmap Identity = Identity


3) Composition
sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA


-}



--- 21.11 TESTING TRAVERSABLE LAWS ------------------------------------------------------

type TI = []

main = do
    let trigger = undefined :: TI (Int, Int, [Int])
    quickBatch (traversable trigger)
{-

    traversable:
      fmap:    +++ OK, passed 500 tests.
      foldMap: +++ OK, passed 500 tests.-}
