import Prelude hiding (flip)

f :: a -> b -> c
f a b = show a ++ " " ++ show b


f' :: b -> a -> c
f' = \b a -> f a b

flip :: (a -> b -> c) -> (b -> a -> c)
flip f = f'