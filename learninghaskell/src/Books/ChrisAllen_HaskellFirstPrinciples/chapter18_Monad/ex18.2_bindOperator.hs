import Control.Monad (join)


bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x


andOne x = [x,1]


main = do
    print $ bind andOne [1,2,3,4,5]