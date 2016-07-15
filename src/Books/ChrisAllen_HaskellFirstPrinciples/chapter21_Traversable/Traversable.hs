
import Data.Foldable
import Data.Monoid
import Data.Functor.Identity

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
s6 = sequenceA (Right (Just [1,2]))

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
ex2 = 