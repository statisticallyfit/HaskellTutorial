import Control.Monad ((>>=))


-- (>>=)    :: Monad m => m a        -> (a -> m b) -> m b

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = fmap f xs