import Control.Monad (join)


j :: Monad m => m (m a) -> m a
j = join

t1 = j [[1,2],[], [3]]
t2 = j (Just (Just 1))
t3 = j (Just Nothing)
t4 = j Nothing

-- HELP why no instance for show?