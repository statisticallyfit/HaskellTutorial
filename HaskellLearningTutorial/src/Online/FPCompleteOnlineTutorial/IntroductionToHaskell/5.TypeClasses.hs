-- todo: need help understanding the error
--Couldn't match expected type `a' with actual type `Bool'
--        `a' is a rigid type variable bound by
--            the type signature for f :: a -> a -> a
{-f :: a -> a -> a
f x y  = x && y
-}



-- TYPE CLASSES

{-
--Num, Eq, Ord, and Show are type classes, and we say that (==), (<),
--and (+) are "type-class polymorphic". Intuitively, type classes
--correspond to sets of types which have certain operations defined for
--them, and type class polymorphic functions work only for types which
--are instances of the type class(es) in question. As an example, let's
--look in detail at the Eq type class.

--class Eq a where
--  (==) :: a -> a -> Bool
--  (/=) :: a -> a -> Bool
--We can read this as follows: Eq is declared to be a type class with a
--single parameter, a. Any type a which wants to be an instance of Eq
--must define two functions, (==) and (/=), with the indicated type
--signatures. For example, to make Int an instance of Eq we would have
--to define (==) :: Int -> Int -> Bool and (/=) ::
--Int -> Int -> Bool. (Of course, there's no need, since the standard
--Prelude already defines an Int instance of Eq for us.)

--Let's look at the type of (==) again:

--(==) :: Eq a => a -> a -> Bool
--The Eq a that comes before the => is a type class constraint. We can
--read this as saying that for any type a, as long as a is an instance
--of Eq, (==) can take two values of type a and return a Bool. It is
--a type error to call the function (==) on some type which is not an
-- instance of Eq. If a normal polymorphic type is a promise that the
-- function will work for whatever type the caller chooses, a type
-- class polymorphic function is a restricted promise that the function
-- will work for any type the caller chooses, as long as the chosen type
-- is an instance of the required type class(es).

-- When any type class method (like ==) is used, the compiler uses
-- type inference to determine which implementation of == should be
-- chosen.
-}


-- todo: how to use this to test output
data Foo = F Int | G Char
    deriving Show

instance Eq Foo where
    (F i1) == (F i2) = i1 == i2
    (G c1) == (G c2) = c1 == c2
    _ == _ = False
    foo1 /= foo2 = not (foo1 == foo2)
-- whenever making an instance of Eq you must either define (==) or
-- (/=), the other will be automatically defined in terms of the one
-- we specify. todo: why infinite recursion if don't specify either one?


-- Another way to do it, generates instances of Eq, Ord, Show for Foo.
data Foo' = F' Int | G' Char
    deriving (Eq, Ord, Show)







-- Type class example

--The "=>" means "for any type a, as long as a is an instance of
-- Listable, function can take a value of type a and return a list of Ints


data Tree a = Empty | Node a (Tree a) (Tree a)


class Listable a where
    toList :: a -> [Int]

instance Listable Int where
    --toList :: Int -> [Int] -- todo: why gives error illegal type signature?
    toList x = [x]

instance Listable Bool where
    --toList :: Bool -> [Bool]
    toList True  = [1]
    toList False = [0]

instance Listable (Tree Int) where
    toList Empty    = []
    toList (Node x l r) = toList l ++ [x] ++ toList r

myTree :: Tree Int
myTree = Node 1 (Node 2 Empty (Node 3 Empty Empty)) (Node 4 Empty Empty)

{-# LANGUAGE FlexibleInstances #-} --todo: what does this mean?
instance Listable [Int] where
    toList = id -- [Int] -> [Int], no work needed to write this one.
-- todo: why error with illegal instance declaration?


instance (Listable a, Listable b) => Listable (a, b) where
    toList (x, y) = toList x ++ toList y




-- functions Listable
sumL x = sum (toList x)  -- uses hardcoded function toList since no other choice

-- foo :: (Listable a, Ord a) => a -> a -> Bool
foo x y = sum (toList x ) == sum (toList y) || x < y


main = print (toList (3::Int), toList myTree)
--print (foo ([2,3,5,7]::[Int]) ([3,6,9]::[Int]))
--print (sumL myTree) -- todo: error
--print (toList myTree) -- todo: error
--print (toList True, toList (7 :: Int)) -- means declaring value 7 with type Int
--print (f 1 1) -- todo: error