

newtype Reader r a = Reader {runReader :: r -> a}


--- 1
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f gx gy = f <$> gx <*> gy



--- 2
asks :: (r -> a) -> Reader r a
asks f = Reader $ \x -> f x