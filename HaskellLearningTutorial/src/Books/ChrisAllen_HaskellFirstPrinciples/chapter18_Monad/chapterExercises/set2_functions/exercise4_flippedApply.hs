import Control.Applicative ((<*>))


a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

t1 = (Just 1) `a` (Just (+10))