
-- HELP how to implement with foldr?
switchMap :: Num a => (a -> a) -> (a -> a) -> [a] -> [a]
switchMap _ _ []       = []
switchMap f _ [x]      = [f x]
switchMap f g (a:b:cs) = f a : g b : switchMap f g cs


-- help why is end type = b?
switchMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
switchMap' _ _ [] = []
switchMap' f g (x:xs) = f x : switchMap' g f xs

main = do
    print $ switchMap (+1) (+10) [1,2,3,4]
    print $ switchMap (+1) (+10) [1,2,3,4,5]
    print $ switchMap' (+1) (+10) [1,2,3,4]
    print $ switchMap' (+1) (+10) [1,2,3,4,5]