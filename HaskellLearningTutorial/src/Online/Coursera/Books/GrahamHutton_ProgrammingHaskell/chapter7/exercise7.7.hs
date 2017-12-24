import Prelude hiding (map, iterate)
import Data.List hiding (map, iterate)
import Data.Char


ff = \x -> 2*x
test p f xs = [f x | x <- xs, p (f x)]


type Bit = Int


-- produces the empty list of px is true and otherwise makes non-empty list
-- by applying function h to give the head and function (t) to give the tail.
unfold p h t x | p x       = []
               | otherwise = h x: unfold p h t (t x)

-- from integer to binary ----------------------------------------------------------
intToBin   :: Int -> [Bit]
intToBin   = unfold (== 0) (`mod` 2) (`div` 2)
--intToBin 0 = []
--intToBin n = n `mod` 2 : intToBin (n `div` 2)

chopEight :: [Bit] -> [[Bit]]
chopEight = unfold null (take 8) (drop 8)
--chopEight []   = []
--chopEight bits = take 8 bits : chopEight (drop 8 bits)) (drop 8)

map   :: (a -> b) -> [a] -> [b]
map f = unfold null (f . head) (tail)


iterate :: (a -> a) -> a -> [a]
iterate f = unfold (const False) id f
-- help: todo: understand this better

main = do
    print $ intToBin 13
    print $ chopEight ([1,0,1,1,1,1,0,0,1,1,0,1,0,1,0,1,1,1,1,1,0,0,0,1])
    print $ map isAlpha ['a', 'b', '1', 'c']
    print $ iterate (*2) 1