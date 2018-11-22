import Control.Monad (liftM)


l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM

t1 = l1 (+1) [1,2,3]