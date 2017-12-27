import Prelude hiding (curry, uncurry)


uf       :: (Int, Int) -> Int
uf (a,b) = 2*a + 3*b

cf     :: Int -> Int -> Int
cf a b = 2*a + 3*b

curry   :: ((a,b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x,y)

uncurry   :: (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(x,y) -> f x y


-- help: todo: understand why these are backwards

main = do
    print $ curry uf 1 2
    print $ uncurry cf (1,2)