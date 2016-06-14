
{-
curry :: ((a,b) -> c) -> a -> b -> c
curry g x y = g (x, y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y
-}


curryList :: ([a] -> d) -> a -> [a] -> d
curryList f = \x xs -> f (x:xs)

-- note the function f is still typed :: [a] -> d so we have to pass
-- the x:xs together not separate.
curryList' :: ([a] -> d) -> a -> [a] -> d
curryList' f x xs = f (x:xs)



uncurryList :: (a -> [a] -> d) -> [a] -> d
uncurryList f = \(x:xs) -> f x xs

uncurryList' :: (a -> [a] -> d) -> [a] -> d
uncurryList' f (x:xs) = f x xs


sumC   :: Num a => a -> [a] -> a
sumC n = (+ n) . sum

main = do
    print $ curryList sum 1 [2,3,4]
    print $ uncurryList sumC [1,2]