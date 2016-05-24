import Data.List (elemIndex)
import Control.Applicative



-- 1
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])



-- 2
{-
= (,) <$> (Just 6) <*> (Just 5)
= (Just (6, )) <*> (Just 5)
= Just (6, 5)
-}
w :: Maybe Integer
w = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> w <*> z




-- 3
{-
note is applied something like this:
= max <$> (Just 2) <*> (Just 3)
= (Just (max 2 _)) <*> (Just 3)
= Just 3

note
(<*>) ::   f   (a -> b) ->    f   a ->    f  b
(<*>) :: Maybe (max)     -> Maybe a -> Maybe b
-}
x :: Maybe Int
x = elemIndex 3 [1..5]

y :: Maybe Int
y = elemIndex 4 [1..5]

maxed :: Maybe Int
maxed = max <$> x <*> y

maxed' = liftA2 max x y

maxed'' = pure max <*> x <*> y








-- 4
ps = [1,2,3]
qs = [4,5,6]

p :: Maybe Integer
p = lookup 3 $ zip ps qs

q :: Maybe Integer
q = lookup 2 $ zip ps qs

summed :: Maybe Integer
summed = sum <$> ( (,) <$> p <*> q)

summed' = pure sum <*> ( (,) <$> p <*> q)

summed'' = sum <$> liftA2 (,) p q



main = do
    -- 1
    print $ added
    -- 2
    print $ tupled
    -- 3
    print $ maxed; print $ maxed'; print $ maxed''
    -- 4
    print $ summed; print $ summed'; print $ summed''