import Control.Monad (join)
import Control.Applicative (pure)


meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (x:xs) f = (++) <$> (fmap (\a -> [a]) $ f x) <*> (meh xs f)


flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs (join . pure)

flipType' xs = meh xs id

{-
NOTE steps

1. take (Just 2) and apply (join . pure) to it to get (Just 2)

2. fmap listify to get (Just [2])

3. fmap append to get (Just ([2] ++))

4. now apply this function to the rest: (Just ([2] ++ [5] ++ [6]))
= Just [2,5,6]
-}
main = do
    print $ flipType [Just 2, Just 5, Just 10]
    print $ flipType' [Just 2, Just 5, Just 10]