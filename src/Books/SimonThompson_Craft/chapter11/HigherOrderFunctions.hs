import Test.QuickCheck
import Prelude hiding (curry, uncurry, flip)

{-
NOTE
-- COMPOSITION
Type: (.) :: (b -> c) -> (a -> b) -> (a -> c)
associative (f . g) . h = f . (g . h)
Takes the function on the right and applies it first.


-- FORWARD COMPOSITION >.>
Takes function on left to apply it first (opposite of above)
Arrows show in which direction information is flowing (from first func to second).

note - our definition not  a lib func
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
(g >.> f) x = (f.g) x


-- APPLICATION OPERATOR, used when
* instead of ()
* used as function : zipWith ($) [sum, product] [[1,2], [3,4]]
-}



-- 11.2 LAMBDA ---------------------------------------------------------------------

mapFuns :: [a -> b] -> a -> [b]
mapFuns [] x = []
mapFuns (f:fs) x = f x : mapFuns fs x


mapFuns' :: [a -> b] -> a -> [b]
mapFuns' fs x = map (\f -> f x) fs


-- PLUMBING FUNCTIONS TOGETHER
-- g (f x) (f y)
--                 f              g       the arg nums a = 3, a = 4, c = result of g
composeTwo :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
composeTwo f g = (\x y -> g (f x) (f y))



-- 11.3. PARTIAL APPLICATION -------------------------------------------------------

doubleAll :: [Int] -> [Int]
doubleAll = map (*2) -- xs

-- not always possible to make a lambda abstraction since argument is inside:
-- elem ch whitespace
 -- note answer ---------------------------------------
whitespace = " \t\n"

isElem :: Char -> Bool
isElem = member whitespace
    where member xs x = elem x xs

-- note or
isElem' = \ch -> elem ch whitespace

-- other examples ---------------------------------------
-- flipV = map reverse
-- beside = zipWith (++)
-- dropSpace = dropWhile (member whitespace)
-- dropWord = dropWhile (not . member whitespace)
-- getWord = takeWhile (not . member whitespace)







-- 11.4 CURRIED FUNCTIONS ---------------------------------------------------------

-- note
-- every function returns a function
-- curried means non-tupled args.

multiply :: Int -> Int -> Int
multiply x y = x * y
-- (multiply 2) 5 = 10

multiplyUC :: (Int, Int) -> Int
multiplyUC (x,y) = x*y



-- NOTE writing a currier function
-- note IMPORTANT:
-- input: function g which is uncurried and has type ((a,b) -> c)
-- output: function g which is curried and has type (a -> b -> c)
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
(curry multiplyUC) 2 10
20
(uncurry multiply) (3,4)
12
(curry (uncurry multiply)) 3 4
12
-}

propZip xs = uncurry zip (unzip xs) == xs










-- More composition --------------------------------------------------------------

succs n = n + 1

twice   :: (a -> a) -> a -> a
twice f = f . f

eval = twice succs 12


-- generalizing twice:
iter :: Integer -> (a -> a) -> a -> a
iter n f
    | n > 0     = f . iter (n - 1) f
    | otherwise = id

eval2 = iter 10 succs 12








-- LAMBDA, Partial application ----------------------------------------------------

-- example 1
flip :: (a -> b -> c) -> b -> a -> c
flip f = \x y -> f y x

-- example 2
getEvens :: [Int] -> [Int]
getEvens = filter ((== 0) . (`mod` 2)) -- xs here



{-
example 3

getWord xs = getUntil ( {-x here-}`elem` whitespace) xs
           = getUntil (  `elem` whitespace) xs
-}







-- EXTENSIONALITY, INTENSIONALITY -------------------------------------------------
-- note extensionality: black boxish, says two functions are the same if they have
-- the same value at every point.
-- note intensionality: NOT black box, says two funcs are same if they have the
-- same definition.




-- HIGHER LEVEL PROOFS ------------------------------------------------------------


-- Example 1: map and composition

{-
1. BASE CASE

LEFT
map (f . g) [] = []

RIGHT
(map f . map g) []
= map f (map g [])
= map f []
= []


2. INDUCTION HYPOTHESIS:
map (f . g) xs = (map f . map g) xs


3. INDUCTION STEP

LEFT
map (f . g) (x:xs)
= (f . g) x : map (f . g) xs
= f (g x) : (map f . map g) xs

RIGHT
(map f . map g) (x:xs)
= map f (map g (x:xs))
= map f (g x : map g xs)
= f (g x) : map f (map g xs)
= f (g x) : (map f . map g) xs

-}



-- Example 2  - map and filter

{-
NOTE

Proposition: (filter p . map f) xs = (map f . filter (p . f)) xs


1. BASE CASE

LEFT
(filter p . map f) []
= filter p (map f [])
= filter p []
= []

RIGHT
(map f . filter (p . f)) []
= map f (filter (p . f) [])
= map f []
= []




3. INDUCTION STEP

LEFT
(filter p . map f ) (x:xs)
= filter p (map f (x:xs))
= filter p (f x: map f xs)

case 1: p (f x) == True
= f x : filter p (map f xs)
= f x : (map f . filter (p . f)) xs


RIGHT
(map f . filter (p . f)) (x:xs)
= map f (filter (p . f) (x:xs))
= map f (x : (filter (p . f) xs))
= f x : map f (filter (p . f) xs)
= f x : (map f . filter (p . f)) xs

-}



-- NOTE quickchecking map and filter
propMapFilter p f = \xs -> (filter p . map f) xs == (map f . filter (p . f)) xs

{-
quickCheck (propMapFilter (>0) (^2))
+++ OK, passed 100 tests.
-}





-- Example 3 flipV and flipH

{-
flipH = reverse
flipV = map reverse

Proposition: map f (reverse xs) = reverse (map f xs)


1. BASE CASE

LEFT
map f (reverse [])
= map f []
= []

RIGHT
reverse (map f [])
= reverse []
= []




3. INDUCTION STEP:

LEFT
map f (reverse (x:xs))
= map f (reverse xs ++ [x]) --- note must prove (map f (ys ++ zs)) = map f ys ++ map f zs
= map f (reverse xs) ++ map f [x]
= map f (reverse xs) ++ [f x]
= reverse (map f xs) ++ [f x] --- note by induction hypothesis


RIGHT
reverse (map f (x:xs))
= reverse (f x : map f xs)
= reverse (map f xs) ++ [f x]
-}