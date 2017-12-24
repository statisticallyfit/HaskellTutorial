
-- note given func a -> c and func b -> d and either a b and return either c d
join :: (a -> c) -> (b -> d) -> Either a b -> Either c d
join f g (Left x) = Left (f x)
join f g (Right y) = Right (g y)



main = do
    print $ join (+1) (++" hello!") (Left 10)
    print $ join (+1) (++" hello!") (Right "spring")
