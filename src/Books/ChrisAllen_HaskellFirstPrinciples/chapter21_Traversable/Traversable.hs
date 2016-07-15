
import Data.Foldable
import Data.Monoid

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

traverse :: (a -> f b) -> t a -> f (t b)       (Applicative f)
mapM     :: (a -> m b) -> t a -> m (t b)       (Monad m)
sequence :: t (m a)    -> m (t a)

traverse_ :: (a -> f b) -> t a -> f ()
mapM_     :: (a -> m b) -> t a -> m ()
sequence_ :: t (m a) -> m ()


key longer version ------------------------------------------------------------------------

traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
mapM :: (Monad m, Traversable t)           => (a -> m b) -> t a -> m (t b)
sequence :: (Monad m, Traversable t)       => t (m a)    -> m (t a)

traverse_ :: (Applicative f, Foldable t) => (a -> f b) -> t a -> f ()
mapM_ :: (Monad m, Foldable t)           => (a -> m b) -> t a -> m ()
sequence_ :: (Monad m, Foldable t)       => t (m a)    -> m ()
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
