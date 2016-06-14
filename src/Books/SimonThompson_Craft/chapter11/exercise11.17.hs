

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 g x y z = g (x, y, z)


uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (x,y,z) = f x y z

-- HELP mull over how to do it with uncurry and curry:
-- https://github.com/pascal-knodel/haskell-craft/blob/master/Chapter%C2%A011/E'11'17.hs
