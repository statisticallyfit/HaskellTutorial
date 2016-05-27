import Control.Monad (liftM2)


l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

t1 = l2 (,) [1,2,3] [4,5]