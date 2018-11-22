
-- todo: why doesn't parametrization with Int work (uncomment the comment)
data IntList {-Int-} = Empty | Cons Int IntList
    deriving Show


myIntList = Cons 2 (Cons (-3) (Cons 5 Empty))


-- RECURSION PATTERN # 1: MAP
addOneToAll :: IntList -> IntList
addOneToAll Empty = Empty
addOneToAll (Cons x xs) = Cons (x + 1) (addOneToAll xs)


absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)


squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x * x) (squareAll xs)


-------------------------------------------------
-- *** The function f takes Int and returns Int (Int -> Int)
--- the function mapIntList takes this function and also takes and IntList
-- and returns an IntList modified.
-- So mapIntList has two arguments: the arguments of f and the IntList argument,
-- and it returns IntList
-- todo: look at part 4 which says that functions only have one argument. How
-- todo: to reconcile the two definitions?

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty    = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)
-------------------------------------------------


-- Using map to implement addOneToAll, absAll, and squareAll

addOne x = x + 1
square x = x * x

addOneToAll' xs = mapIntList addOne xs
absAll' xs = mapIntList abs xs
squareAll' xs = mapIntList square xs




----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------


-- RECURSION PATTERN # 2: Filter

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
    | even x        = Cons x (keepOnlyEven xs) -- Cons has arguments Int, IntList
    | otherwise     = keepOnlyEven xs



----------------------------------------------------------------
filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList p (Cons x xs)
    | p x       = Cons x (filterIntList p xs)
    | otherwise = filterIntList p xs
----------------------------------------------------------------



----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------

--- RECURSION PATTERN # 3: FOLD
sum' :: [Integer] -> Integer
sum' []     = 0
sum' (x:xs) = x + sum' xs


product' :: [Integer] -> Integer
product' []     = 0
product' (x:xs) = x * product' xs


length' :: [a] -> Int  -- todo: why these types?
length' []     = 0
length' (_:xs) = 1 + length' xs



-- todo: need help to understand the meaning of the signature
fold :: (a -> b -> b) -> b -> [a] -> b
fold f z []     = z
fold f z (x:xs) = f x (fold f z xs)

-- functions defined in terms of fold
sum''       = fold (+) 0
product''   = fold (*) 1
length''    = fold(\_ s -> 1 + s) 0
-- todo: what does \_ s ... mean?
-- could be replaced with: (\_ -> (1+)) or (const (1+)) todo: meaning?


inputList = [1, 2, 3, 4, 5]


main = print ( sum'' inputList, product'' inputList, length'' inputList)

----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------



--- POLYMORPHISM
-- (polymorphic data types good for general types)

-- data List t means that List is parametrized by a type.
data List t = E | C t (List t)
    deriving Show
-- same as IntList: Cons takes a variable with a type t plus a list with that type


l1 :: List Int
l1 = C 3 (C 5 (C 2 E ))

l2 :: List Char
l2 = C 'x' (C 'y' (C 'z' E))

l3 :: List Bool
l3 = C True (C False E)




--- Generalizing filter function
filterList :: (t -> Bool) -> List t -> List t -- GHCI infers this, no need to include it
filterList _ E = E
filterList p (C x xs)
    | p x       = C x (filterList p xs)
    | otherwise = filterList p xs


myList = C 2 (C (-3) (C 5 E))



---- Generalizing map function
-- the most general map since we want to map Int to String
mapList :: (a -> b) -> List a -> List b
mapList f (C x xs) = C (f x) (mapList f xs)
mapList f E        = E

-- function double that takes x and doubles it
-- what does it type mean: Num a => a -> a
double x = 2 * x





-- NOTE never use partial functions (they fail for some input) always use
-- pattern matching. Or if you need to write partials, write them safely:
-- Change output type to indicate possible failure

--data Maybe a = Nothing | Just a --- already in Haskell


emptyStringList :: [String]
emptyStringList = []

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:_)  = Just x




-- Make types reflect the problem

------ Haskel Definition
data NonEmptyList a = NEL a [a]

--- the NEL constructor takes two arguments a, and [a]
--- the data is parametrized by type (a)

--import Data.List.NonEmpty hiding (NonEmptyList)
{-
nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNEL :: [a] -> Maybe (NonEmptyList a )
listToNEL []     = Nothing
listToNEL (x:xs) = Just (NEL x xs)

headNEL :: NonEmptyList a -> a
headNEL (NEL x _) = x

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ xs) = xs



-- todo: how to resolve this error? Do I need to import NonEmptyList and hide it?


-- declaring the nelList variable to convert into list
nelList :: NonEmptyList Char
nelList = NEL ['a', 'b', 'c', 'd'] --["list", "of", "words"]
-- declaring the list variable to convert into NEL
list = [1,2,3,4,5,6,7]
-}


--main = print (listToNEL nelList)
-- print (safeHead emptyStringList, safeHead ["hello"])
-- print (mapList double myList)
--print(filterList even myList)
    --print(l1)
    --print(l2)
    --print(l3)
--print (filterIntList even myIntList)
--print (keepOnlyEven myIntList)
--print (addOneToAll' myIntList)
--print (absAll myIntList)
--print (addOneToAll myIntList)