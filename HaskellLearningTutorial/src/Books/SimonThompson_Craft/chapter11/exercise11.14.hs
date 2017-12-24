import Prelude hiding (curry, uncurry)

-- g :: ((a,b) -> c)
-- x :: a
-- y :: b
-- g (x,y) :: c
-- :t (curry multiplyUC) :: Int -> Int -> Int is the type
-- because the args ((a,b) -> c) are canceled out with ((Int, Int) -> Int)
-- leaving the remaining (a -> b -> c) which is (Int -> Int -> Int) type.
curry :: ((a,b) -> c) -> a -> b -> c
curry g x y = g (x, y)

-- note
-- f :: (a -> b -> c)
-- (x,y) :: (a,b)
-- f x y :: c
-- :t (uncurry multiply) :: (a,b) -> c
-- because the types (Int -> Int -> Int) which is (a -> b -> c) cancel out.
uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y


{-
NOTE

($)     :: (a -> b) -> a -> b
uncurry :: (   a    -> b -> c) -> (a,b) -> c

:t (uncurry ($))
(a -> b, a) -> b

-------------------------------------------------

(:)     ::  a -> [a] -> [a]
uncurry :: (a -> b   ->  c)  -> (a,b) -> c

:t (uncurry (:))
(a, [a]) -> [a]

-------------------------------------------------

(.)     :: (b -> c) -> (a -> b) -> a -> c
uncurry :: (    a    ->     b   -> c) -> (a,b) -> c

:t (uncurry (.))
((b -> c), (a -> b)) -> a -> c

NOTE figured these out by cancelling/matching out the types above.
step 1 - match the a,b,c in uncurry to the type of the function.
step 2 - now that we know what a,b,c are, write out (a,b) -> c in terms of their
new values.
-}