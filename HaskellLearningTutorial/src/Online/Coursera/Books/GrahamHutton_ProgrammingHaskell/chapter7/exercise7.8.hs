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




-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-- note: exercise 7.8

parity :: [Bit] -> Int
parity bits
    | odd (sum bits) = 1
    | otherwise      = 0

addParity :: [Bit] -> [Bit]
addParity bits = (parity bits) : bits


checkParity :: [Bit] -> [Bit]
checkParity (b:bs)
    | b == parity bs = bs
    | otherwise      = error "parity mismatch"

-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------


encode :: String -> [Bit]
encode = concat ◦ map (addParity ◦ makeEight ◦ intToBin ◦ ord) -- the args are here next to this
-- composed function so this composed function is mapped over the args.


-- chop a list into eight-bit binary numbers
chopNine      :: [Bit] -> [[Bit]]
chopNine []   = []
chopNine bits = take 9 bits : chopNine (drop 9 bits)


decode :: [Bit] -> String
decode = map (chr ◦ binToInt ◦ checkParity) ◦ chopNine


-------------------------------------------------------------------------------------
transmit :: String -> String
transmit = decode ◦ channel ◦ encode

-- exercise 7.9

channel        :: [Bit] -> [Bit]
channel (b:bs) = bs


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
    print $ chopNine (encode "abc")
    print $ chopNine (encode "abcd")
    putStrLn "" -----------------------
    print $ decode (encode "abc")
    putStrLn "" -----------------------
    print $ transmit "higher-order functions are easy"
    putStrLn "" -------------------------------------------- exercise 7.9
    --print $ channel [1,0,1,1,1]