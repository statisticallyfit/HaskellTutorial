import Control.Applicative

hurr = (*2)
durr = (+10)

m :: Integer -> Integer
m = hurr . durr



-- note called the functor of functions
m1 :: Integer -> Integer
m1 = fmap hurr durr -- equals (*2) ((+10) x)


m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr
-- equals ((+) <$> (3*2) <*> (3+10)) => ((3 * 2) + (3 + 10))

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr -- same as above.


{-
NOTE

example
((+) . (*2)) 5 3
13
example
((+) <$> (*2)) 5 3
13

key
((+) . (*2)) == \x -> (+) (2 * x)

1) note:
((+) . (*2)) 5 3
= (\x -> (+) (2 * x)) 5 3
= (\5 -> (+) (2 * 5)) 3
= ((+) 10) 3
= 13


2) note
((+) <$> (*2) <*> (+10)) 3
= ((+) <$> (3*2) <*> (3+10))
= ((3 * 2) + (3 + 10))



-}


-- NOTE: intuition for Reader: Reader is a way to string functions together when
-- they are waiting for input from shared environment.
-- Using Reader avoids passing that argument around explicity.

-- example
(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) f g x = ((||) <$> f <*> g) x
-- or note:
-- (<||>) = liftA2 (||)



-- example
-- help help help understand why monadic context is used?
hurrDurr :: Integer -> Integer
hurrDurr = do
    a <- hurr -- argument gets applied to hurr resulting in integer (a)
    b <- durr -- arg applied to durr, resulting in integer (a).
    return (a + b) -- integers are added.
