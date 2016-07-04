import Prelude hiding (sum, length, product, concat)
import Test.QuickCheck



{-
NOTE: definition of foldr: puts the accumulator at the end.

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

example: foldr (+) 0 [1,2,3] --- > (1+(2+(3+0)))
-}

sum :: [Integer] -> Integer
sum [] = 0
sum (x:xs) = x + sum xs

length :: [a] -> Integer
length [] = 0
length (_:xs) = 1 + length xs

product :: [Integer] -> Integer
product [] = 1
product (x:xs) = x * product xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs



-- note compare this new definition of foldr with the one above.
foldrNew :: (a -> b -> b) -> b -> [a] -> b
foldrNew f acc xs =
    case xs of
        []     -> acc
        (x:xs) -> f x (foldrNew f acc xs)

{-

NOTE look at the steps using function above.

foldr (+) 0 [1,2,3]

= foldr (+) 0 [1,2,3]
    case [1,2,3] of
        []          -> 0
        (1 : [2,3]) -> (+) 1 (foldr (+) 0 [2,3])

--- note there is (+) 1 implicitly wrapped around this continuation of fold.
--- > foldr (+) 0 [2,3]
        case [2,3] of
            []      -> 0
            (2:[3]) -> (+) 2 (foldr (+) 0 [3])

-- note there is (+) 1 ((+) 2 ...) implicitly wrapped around this continuation of fold.
--- > foldr (+) 0 [3] =
        case [3] of
            []       -> 0
            (3 : []) -> (+) 3 (foldr (+) 0 [])

-- note there is (+) 1 ((+) 2 ((+) 3 ...)) implicitly wrapped around this
-- continuation of fold.
--- > foldr (+) 0 [] =
        case [] of
            [] -> 0 --- equals matched
-}

-- note IMPORTANT seeing how foldr evaluates:
xs = map show [1.. 5]

f :: String -> String -> String
f x y = concat ["(", x, "+", y, ")"]

evalFoldr = foldr f "0" xs











------------------------------------------------------------------------------------------
{-

NOTE definition of foldl: puts the accumulator at the beginning.

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

example: foldl (+) 0 [1,2,3] --- > (((0+1)+2)+3)
-}

--xs = map show [1..5]
--f x y = concat ["(", x, "+", y, ")"]
evalFoldl = foldl f "0" xs


-- note see difference between scanr and scanl
exFoldr1 = foldr (+) 0 [1..5]
exScanr = scanr (+) 0 [1..5]

exFoldl1 = foldl (+) 0 [1..5]
exScanl = scanl (+) 0 [1..5]

{-
NOTE relationships between the scans and folds:

last (scanl f z xs) = foldl f z xs
head (scanr f z xs) = foldr f z xs


NOTE meaning of folding:
answer: foldr means replacing the cons (:) with the function (f) given.

[1..3] == 1 : 2 : 3 : []

foldr f z [1,2,3]
1 `f` (foldr f z [2,3])
1 `f` (2 `f` (foldr f z [3]))
1 `f` (2 `f` (3 `f` (foldr f z [])))
1 `f` (2 `f` (3 `f` z))



Example (non-associatve function)

foldr (^) 2 [1..3]
= (1 ^ (2 ^ (3 ^ 2)))
= (1 ^ (2 ^ 9))
= 1 ^ 512
= 1

foldl (^) 2 [1..3]
= ((2 ^ 1) ^ 2) ^ 3
= (2 ^ 2) ^ 3
= 4 ^ 3
= 64

-}

xs' = map show [1..3]
g x y = concat ["(", x, "^", y, ")"]
evalFoldr2 = foldr g "2" xs'
evalFoldl2 = foldl g "2" xs'


{-
NOTE meaning of arguments:

foldr :: (a -> b -> b) -> b -> [a] -> b
         [1]  [2]  [3]
foldl :: (b -> a -> b) -> b -> [a] -> b
         [4]  [5]  [6]

1 => (a) is one of the list args that foldr is applied to.
2 => (b) is the next value that will be used to get accumulated in fold.
3 => this (b) is final result of having combined (a) and starting (b) into the fold.
4 => start value or fold accumulated so far.
5 => the next element in the list [a]
6 => the final result of foldl's fold.
-}

exFoldr3 = foldr const 0 [1..5]
exFoldr4 = foldr (flip const) 0 [1..5]
exFoldl3 = foldl const 0 [1..5]
exFoldl4 = foldl (flip const) 0 [1..5] -- note so now (flip const) returns second arg.





{-

-- note no error because const returns first not second (undefined)
*Main> foldl const 0 ([1..5] ++ [undefined])
0

-- note error because flip const returns second which gets to be undefined.
*Main> foldl (flip const) 0 ([1..5] ++ [undefined])
*** Exception: Prelude.undefined

-- note error because foldl must evaluate spine (the []) and undefined is part of spine
-- here
*Main> foldl (\_ _ -> 5) 0 ([1..5] ++ undefined)
*** Exception: Prelude.undefined

-}










--- 10.6 HOW TO WRITE FOLD FUNCTIONS ------------------------------------------------------

-- note write function that takes first three letters of each String in a list of Strings
-- and concats that result into a final string


{-
NOTE compare foldr and foldl ---------------------------

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs



Note these are wrong
*Main> let pab = ["Pizza", "Apple", "Banana"]
*Main> foldl (\a b -> take 3 a) "" pab
""
*Main> foldl (\acc b -> take 3 b) "" pab
"Ban"
*Main> foldr (\a acc -> take 3 acc) "" pab
""
*Main> foldr (\a acc -> take 3 a) "" pab
"Piz"


NOTE these are the answers
IMPORTANT SUMMARY KEY NOTE of which arg is the accumulator in foldr/foldl.

-- key acc comes second for foldr
foldr (\a acc -> take 3 a ++ acc) "" pab
"PizAppBan"

-- key acc comes first for foldl.
foldl (\acc b -> take 3 b ++ acc) "" pab
"BanAppPiz"


example

*Main> foldr g "1" xs
"(0^(7^(3^(6^(0^1)))))"

*Main> foldl g "1" xs
"(((((1^0)^7)^3)^6)^0)"

-------------------------------------------------
-}














--- 10.7 FOLDING AND EVALUATION ---------------------------------------------------------

-- note help todo: do the types int and list of ints make for some superfluous testing?
-- note made function non-associative, unlike (+) or (*).
-- HELP why does this still fail due to negative numbs? I abs'd all of them, no? 
testFoldrToFoldl :: Int -> [Int] -> Bool
testFoldrToFoldl seed xs = (foldr (^) seed' xs') == (foldl (flip (^)) seed' (reverse xs'))
    where seed' = abs seed
          xs' = map abs xs

