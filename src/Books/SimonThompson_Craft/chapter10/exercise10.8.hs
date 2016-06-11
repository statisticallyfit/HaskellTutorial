
-- twice :: (a -> a) -> a -> a
twice :: (Integer -> Integer) -> Integer -> Integer
twice f n = f (f n)