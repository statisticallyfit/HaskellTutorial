{-
note TYPE: for making type synonyms

example
type IntList = [Int]
type Tree = (Int, [Tree]) -- wrong! cannot make them recursive!
type Parser a = String -> [(a, String)] -- note can also be parametrized by other types.
type Assoc k v = [(k,v)] -- note can have more than one type parametrizations



note DATA: for making completely new data types - as many constructors and fields as needed.
These are like case class inheritance in scala (tati example)
Can also have variables inside (then they are like classes with fields)
Can also have functions inside.

example
data Shape = Circle Float | Rect Float Float
data Cow = {name :: String, age :: Int, weight :: Float}
data Sum = {getSum :: Int -> Int -> Int }   note can take functions
data Maybe a = Nothing | Just a       note can also be parametrized.
data Nat = Zero | Succ Nat     note can be recursive
data Tree = Leaf Int | Node Tree Int Tree

example
square :: FLoat -> Shape
square n = Rect n n -- here the type of result returned is Shape not Rect, like in OOP.

:t Cirlce
Circle :: Float -> Shape
:t Rect
Rect :: Float -> Float -> Shape




note
NEWTYPES: used to wrap existing types in new types because it is easier to make
them instances of type classes. The wrapped type is separate from the existing type.
Can also have functions inside.
Like data but just with one constructor and one field.

example
newtype CharList = CharList {getCharList :: [Char]} note cannot use ++ to stick
   the CharList and [Char] together.

newtype Sum = {getSum :: Int -> Int -> Int }   note can take functions
-}





data Nat = Zero | Succ Nat deriving (Eq, Show)
-- natural numbers are from 0 -> infinity

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ (intToNat (n-1))


add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n) -- note removing one layer then continuing
-- add m n = intToNat (natToInt m + natToInt n)







{- -- this is a search tree since flattened, it is sorted.
            5
           / \
          /   \
         /     \
       3         7
      / \       / \
    1     4   6     9

-}
data Tree = Leaf Int | Node Tree Tree deriving (Eq, Show)

-- HELp how to declare this tree with numbers at nodes? ???
t1 :: Tree
t1 = Node (Node (Leaf 1) (Leaf 4))    ( Node (Leaf 6)  (Node (Leaf 9) (Node (Leaf 6) (Leaf 7))) )
-- t =   Node    (Node (Leaf 1) 3 (Leaf 4))    5     (Node (Leaf 6) 7 (Leaf 9))

t2 :: Tree
t2 = Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 6) (Leaf 9))




-- whether given number occurs in a tree.
occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r) = m == n || occurs m l || occurs m r


occursInSearchTree :: Int -> Tree -> Bool
occursInSearchTree m (Leaf n) = m == n
occursInSearchTree m (Node l n r)
    | m == n    = True
    | m < n     = occurs m l
    | otherwise = occurs m r


flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

{-
Differently structured trees

data Tree a = Leaf a | Node (Tree a) (Tree a)
data Tree a = Leaf | Node (Tree a) a (Tree a)
data Tree a b = Leaf a | Node (Tree a b) b (Tree a b)
data Tree a = Node a [Tree a]
-}






-- 10.6 Class and instance declarations ---------------------------------------------------

{-
class Eq a where
    (==), (	=)::a → a → Bool
    x /= y = not (x == y)

instance Eq Bool where
    False == False = True
    True == True   = True
    _ == _         = False

    -- note only types using data mechanism can be made into instances of classes.


note classes can be extended Ord is an extension of Eq

class Eq a => Ord a where
    (<), (<=), (>), (>=)  :: a -> a -> Bool
    min, max              :: a -> a -> a
    min x y | x <= y      = x
            | otherwise   = y
    max x y | x <= y      = y
            | otherwise   = x

instance Ord Bool where
    False < True = True
    _ < _        = False
    b <= c       = (b < c) || (b == c)
    b > c        = c < b
    b >= c       = c <= b


    note can do this automatically with deriving (Eq, Ord, Show, Read)
    note ordering of constructors of a type for Ord is determined by their
    position in their declaration. If declaring data Bool = True | False
    then it would be True < False rather than False < True
     note args of a data must also have instances of Eq, Ord ... to use deriving.
-}