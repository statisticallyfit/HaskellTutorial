module ReaderPractice where

import Control.Applicative
import Data.Maybe


x = [1,2,3]
y = [4,5,6]
z = [7,8,9]


-- note
-- lookup :: Eq a => a -> [(a,b)] -> Maybe b

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z



x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> Maybe (Integer, Integer)
x3 n = (,) <$> (z' n) <*> (z' n)




-- note:
-- uncurry :: (a -> b -> c) -> ((a,b) -> c)

summed :: Num c => (c,c) -> c
summed = uncurry (+)

-- just pass in: bolt 1 ==> False, bolt 7 == True
bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)


-- note
-- fromMaybe :: a -> Maybe a -> a
-- fromMaybe 0 xs = 6, fromMaybe 0 zs = 0



-- note
-- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
seqA :: Integral a => a -> [Bool]
seqA m = sequenceA [(> 3), (< 8), even] m


s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)



--- 1
foldAndOverSeq :: Integer -> Bool
foldAndOverSeq val = foldl (&&) True (seqA val)


--- 2
applySeqAToS :: [Bool]
applySeqAToS = seqA $ fromMaybe 0 s'
-- fmap seqA s' :: Maybe [Bool]


--- 3
applyBoltToYs :: Bool
applyBoltToYs = bolt $ fromMaybe 0 ys


--- 4
applyBoltToZ' :: Integer -> Bool
applyBoltToZ' n = bolt $ fromMaybe 0 (z' n)


main :: IO()
main = do
    print $ sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)
    print $ bolt 7
    print $ fmap bolt z
    print $ seqA 7
    print $ foldAndOverSeq 6
    print $ foldAndOverSeq 7
    print $ applySeqAToS
    print $ applyBoltToYs
    print $ applyBoltToZ' 4
