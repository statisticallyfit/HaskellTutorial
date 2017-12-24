import Data.Char
import Data.List (take)
-- Assume binary numers are given in reverse order to normal so the
-- ordinary 1101 is given as 1011

-- from binary to integer -----------------------------------------------------------
type Bit = Int

binToInt :: [Bit] -> Int
binToInt = foldr (\x y -> x + 2*y) 0
--   (1*a) + (2*b) + (4*c) + (8*d)
-- = a + 2 * (b + 2 * (c + 2 * (d + (2 * 0))))

binToInt'      :: [Bit] -> Int
binToInt' bits = sum [w * b | (w,b) <- zip weights bits]
                 where weights = iterate (*2) 1 -- creates infinity even list

-- iterate f x = [x, f x, ffx, fffx, ffffx, ...]

-- from integer to binary ----------------------------------------------------------
intToBin   :: Int -> [Bit]
intToBin 0 = []
intToBin n = n `mod` 2 : intToBin (n `div` 2)

-- make sure all binary numbers are length 8 (8 bits)
makeEight      :: [Bit] -> [Bit]
makeEight bits = take 8 (bits ++ repeat 0)





-- Transmission --------------------------------------------------------------------
(◦) :: (b -> c) -> (a -> b) -> (a -> c)
f ◦ g = \x -> f (g x)
-- could be written f (g x) but it is better in lambda form.

encode :: String -> [Bit]
encode = concat ◦ map (makeEight ◦ intToBin ◦ ord) -- the args are here next to this
-- composed function so this composed function is mapped over the args.


-- chop a list into eight-bit binary numbers
chopEight      :: [Bit] -> [[Bit]]
chopEight []   = []
chopEight bits = take 8 bits : chopEight (drop 8 bits)


decode :: [Bit] -> String
decode = map (chr ◦ binToInt) ◦ chopEight


-------------------------------------------------------------------------------------
transmit :: String -> String
transmit = decode ◦ channel ◦ encode

channel :: [Bit] -> [Bit]
channel = id


main = do
    print $ binToInt [1,0,1,1]
    print $ binToInt [1,0,1,1,0,0,0,0]
    print $ binToInt [1,1,1,1,0,0,0,1]
    putStrLn "" -----------------------
    print $ intToBin 13
    print $ intToBin 1234
    putStrLn "" -----------------------
    print $ makeEight [1,0,1,1]
    putStrLn "" -----------------------
    print $ encode "abc"
    putStrLn "" -----------------------
    print $ chopEight (encode "abc")
    print $ chopEight (encode "abcd")
    putStrLn "" -----------------------
    print $ decode (encode "abc")
    putStrLn "" -----------------------
    print $ transmit "higher-order functions are easy"