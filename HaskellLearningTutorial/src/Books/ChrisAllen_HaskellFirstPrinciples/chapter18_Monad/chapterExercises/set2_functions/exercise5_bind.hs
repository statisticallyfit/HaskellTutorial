import Control.Monad ((>>=))


-- (>>=)    :: Monad m => m a        -> (a -> m b) -> m b

-- note it's like the >>= bind operator without the join
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (x:xs) f = (++) <$> (fmap (\a -> [a]) $ f x) <*> (meh xs f)
{-
NOTE is this correct what it does:

1. (fmap (\a -> [a]) $ f x) what it does is first do (f x) then it fmaps
the list over the argument. Example:
fmap listify $ justify x
= fmap listify $ Just x
= Just [x]

2. Then concatenate that list content in the structure with further list
structure (meh xs f):

Just ([1] ++ [2] ++..) = Just [1,2,..]

-}
-- had help on this one
-- todo review and understand better.


andOne x = [x,1]

listify = \a -> [a]
justify x = Just x

t1 = meh [1..10] (\x -> Right x)
t2 = meh [1..10] (\x -> Left x)

main = do
    print $ fmap andOne [1,2,3]
    --print $ meh [1..10] andOne
    print $ fmap andOne (Just 10) -- m is Just, []
    print $ meh [1..10] justify
