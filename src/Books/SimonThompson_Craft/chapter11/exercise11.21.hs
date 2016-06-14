iter :: Integer -> (a -> a) -> a -> a
iter n f
    | n > 0     = f . iter (n - 1) f
    | otherwise = id



iter'     :: Int -> (a -> a) -> a -> a
iter' n f = composeList
    where fs = replicate n f
          composeList = foldr (.) id fs


-- note these are both partially applied functions because we still have to give the
-- second last 'a' argument.