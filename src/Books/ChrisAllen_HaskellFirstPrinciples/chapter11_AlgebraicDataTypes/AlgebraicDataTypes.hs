{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Int
import Test.QuickCheck
import Control.Monad
import Data.List
import System.Random (Random)


--- > note Trivial type constructor is nullary since it takes no arguments.
-- Called type constants not type constructors.
--- > note Trivial' is a data construcotr.
data Trivial = Trivial' deriving (Eq, Show)


--- > note UnaryTypeCon is type constructor with one argument.
--- > note UnaryValueCon is a data constructor of one argument.
-- Both do not behave like functions. The data constructor is like a box not function.
data UnaryTypeCon a = UnaryValueCon a deriving (Eq, Show)


--- > note PugType is type constructor but it takes no args so it is a type constant.
--- > note PugData is data constructor but it takes no args so it is a constant value.
data PugType = PugData


--- > note HuskyType is type constructor with single parameter.
--- > note HuskyData is data constructor. Does not have arg which means (a) is a phantom
-- which means it has no witness on data constructor level .
data HuskyType a = HuskyData


--- > note DogueDeBordeaux is type constructor with single arg (same as (a) above since
-- names of the variables don't matter)
--- > note DogueDeBordeaux is the lone data constructor. (doge) is witnessed here.
data DogueDeBordeaux doge = DogueDeBordeaux doge


-- note kinds are applied to type constructors NOT data constructors.
-- note types finding is applied to data constructors NOT type constructors.
-- A type is fully applied if its kind is (*). If ( * -> *) or more then it is
-- still awaiting application.
-- note kinds become types when they are fully applied.



--- note making a value of the type of each:
pug = PugData :: PugType

husky :: HuskyType a
husky = HuskyData

otherHusky :: Num a => HuskyType a
otherHusky = HuskyData

anotherHusky :: HuskyType  [[[[[[Int]]]]]]
anotherHusky = HuskyData --- no witness

dog :: DogueDeBordeaux Int
dog = DogueDeBordeaux 10

--badDog :: DogueDeBordeaux String
--badDog = DogueDeBordeaux 10




--- 11.6 DATA CONSTRUCTOR AIRITIES ---------------------------------------------------------

-- nullary - no args in data constructor
data Example0 = Example0

-- unary - 1 arg  in data constructor
data Example1 = Example1 Int

-- product of Int and String - 2 args in data constructor
data Example2 = Example2 Int String




--- 11.7 WHY DATA TYPES ARE ALGEBRAIC ---------------------------------------------------

-- note cardinality of data type is number of possible values it defines.

-- example
--- > data types that contain a unary constructor always have the same cardinality
-- as the type they contain (as many goats as there are ints)
data Cats = Cats Int deriving (Eq, Show)





tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42


class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

-- note cardinality of newtype is same as that of type it contains
-- newtype is special: after comile time, Goat becomes identical to Int.
newtype Goats = Goats Int deriving (Eq, Show, TooMany)
newtype Cows = Cows Int deriving (Eq, Show)

-- the pragma at top of source file is needed to derive TooMany instance automatically.
testTooManyGoats = tooMany (Goats 234)

{-
instance TooMany Goats where
    tooMany (Goats n) = tooMany n -}





--- 11.8 SUM TYPES ---------------------------------------------------------------------

-- sum types mean you have one or fewer args per data constructor.
data NumberOrBool = Numba Int8 | BoolyBool Bool
--- cardinality == 3
-- because cardinality Int8 = 256 (128 + 128)
-- and because cardinality Bool = 2
-- so 2 + 256 = 258 since | means sum type adds.



--- 11.9 PRODUCT TYPES ------------------------------------------------------------------

-- product types mean you have two or more args per data constructor.
data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth deriving (Eq, Show)

-- cardinality is product of the two data constructor arg cardinalities.
-- cardinality of TwoQs == 9
data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)
-- cardinality of TwoQs is again == 9
type TwoQs' = (QuantumBool, QuantumBool)





--- Record types ------------------------

data Person' = MkPerson' String Int deriving (Eq, Show)


-- Records are product types with additional syntax to provide convenient accessors
-- to fields within the record.
-- example record type
data Person = Person { name :: String, age :: Int} deriving (Eq, Show)






--- 11.10 NORMAL FORM ---------------------------------------------------------------------

data Fiction = Fiction deriving (Eq, Show)
data Nonfiction = Nonfiction deriving (Eq, Show)
data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving (Eq, Show)

type AuthorName = String
data Author = Fiction' AuthorName | Nonfiction' AuthorName deriving (Eq, Show)




--- 11.11 CONSTRUCTING / DECONSTRUCTION ------------------------------------------------

data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct {pfirst :: a, psecond :: b} deriving (Eq, Show)


---------------

--- > example 2 values in the product
--- > note Farmhouse and Farmhouse' are the same.
newtype NumCow = NumCow Int
newtype NumPig = NumPic Int
data Farmhouse = Farmhouse NumCow NumPig
type Farmhouse' = Product NumCow NumPig


--- > example 3 values in the product
newtype NumSheep = NumSheep Int

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep) -- alternatively


--- > example 3 values in sum.
type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int
data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo) -- alternatively




------
-- data Sum a b = First a | Second b deriving (Eq, Show)
data Twitter = Twitter deriving (Eq, Show)
data AskFm = AskFm deriving (Eq, Show)

--- > note doesn't work to but AskFm in First and Twitter in Second if we declare
-- them to take their arguments to be Twitter in First and AskFm in Second.
socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter




------

-- this builder pattern doesn't work with record types --- need to define whole record
-- type at once or not at all.

data ThereYet = There Integer Float String Bool deriving (Eq, Show)

-- who needs a "builder pattern"?
nope :: Float -> String -> Bool -> ThereYet
nope = There 10

notYet :: String -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet "woohoo"

yusssss :: ThereYet
yusssss = notQuite False




--------- Deconstructing values

newtype Name' = Name' String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show

data Farmer = Farmer Name' Acres FarmerType deriving Show

--- note unpacks data
isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False


--- alternate formulation with product that uses record syntax
data FarmerRec = FarmerRec { name' :: Name', acres :: Acres, farmerType :: FarmerType}
    deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType farmer of
                            DairyFarmer -> True
                            _ -> False








--- 11.12 FUNCTION TYPE IS EXPONENTIAL ---------------------------------------------------

--- given function a -> b its inhabitants are (b ^ a)
--- given function a -> b -> c its inhabitants are ((c ^ b) ^ a)

data Quantum = Yes | No | Both deriving (Eq, Show)

--- Sums are additive so that has 6 types: ---------------------------------------------
data HasSixValues = Either Quantum Quantum
--- cardinality of Either Q Q == 3 + 3 = 6
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes
quantSum2 = Right No
quantSum3 = Right Both
quantSum4 = Left Yes
quantSum5 = Left No
quantSum6 = Left Both

--- product types are multiplicative so:
--- 3 * 3
data HasNineProductValues = NineVals (Quantum, Quantum) ---------------------------------

quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)
quantProd2 = (Yes, No)
quantProd3 = (Yes, Both)
quantProd4 = (No, Yes)
quantProd5 = (No, No)
quantProd6 = (No, Both)
quantProd7 = (Both, Yes)
quantProd8 = (Both, No)
quantProd9 = (Both, Both)


--- arithmetic of function type: ---------------------------------------------------------
--- Q -> Q means (numQ ^ numQ) means (3 ^ 3)
has27Implementations :: Quantum -> Quantum
has27Implementations = undefined

--- a -> b means b ^ a = 2 ^ 3 = 8
has8Implementations :: Quantum -> Bool
has8Implementations = undefined









--- 11.13 HIGHER KINEDED DATA TYPES ------------------------------------------------------

--- note higher kinded type means kind that has 2 or more (*):
-- example:
-- * -> *
-- * -> * -> *

-- :k is * -> * -> * -> * -> * (because a b c d take 4 and Silly type constructor takes 1)
data Silly a b c d = MkSilly a b c d deriving Show

{-
*Main> :k Silly
Silly :: * -> * -> * -> * -> *
*Main> :k Silly Int
Silly Int :: * -> * -> * -> *
*Main> :k Silly Int Int
Silly Int Int :: * -> * -> *
*Main> :k Silly Int Int Int
Silly Int Int Int :: * -> *
*Main> :k Silly Int Int Int Int
Silly Int Int Int Int :: *
-}




--- 11.15 BINARY TREE ------------------------------------------------------------------

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

t1 = Leaf
t2 = Node (Node (Node Leaf 1 Leaf) 3 (Node Leaf 4 Leaf)) 5
          (Node (Node Leaf 7 Leaf) 13 (Node Leaf 14 (Node Leaf 17 Leaf)))

--- precondition tree must be in binary search order (sorted).
insertTree :: Ord a => a -> BinaryTree a -> BinaryTree a
insertTree b Leaf = Node Leaf b Leaf
insertTree b (Node left a right)
    | b == a = Node left a right
    | b < a  = Node (insertTree b left) a right
    | b > a  = Node left a (insertTree b right)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

collapseTree :: BinaryTree a -> [a]
collapseTree Leaf = []
collapseTree (Node left a right) = collapseTree left ++ [a] ++ collapseTree right


sizeTree :: BinaryTree a -> Int
sizeTree Leaf = 0
sizeTree (Node left _ right) = 1 + sizeTree left + sizeTree right

occurs :: Eq a => a -> BinaryTree a -> Bool
occurs _ Leaf = False
occurs x (Node left a right)
    | x == a = True
    | otherwise = occurs x left || occurs x right


--- TESTING ---------------------------------------------------------------------------


{-instance Arbitrary a  => Arbitrary (BinaryTree a) where
    arbitrary = sized arbBTree
arbBTree 0 = return Leaf
arbBTree n = frequency [(1, return Leaf),
                        (4, liftM3 Node (arbBTree (n `div` 2))
                                        arbitrary
                                        (arbBTree (n `div` 2)) )]-}
instance (Ord a, Bounded a, Random a, Num a, Arbitrary a) => Arbitrary (BinaryTree a)  where
   arbitrary = gen 0 100 where
      gen :: (Ord a, Num a, Random a) => a -> a -> Gen (BinaryTree a)
      gen min max | (max - min) <= 3 = return Leaf
      gen min max = do
        elt <- choose (min, max)
        frequency [ (1, return Leaf),
                    (6, liftM3 Node (gen min (elt - 1))
                                    (return elt)
                                    (gen (elt + 1) max)) ]


isSorted xs = (sort xs) == xs


testMap :: BinaryTree Int -> Bool
testMap tree = (collapseTree $ mapTree (+3) tree) == (map (+3) (collapseTree tree))

testSize :: BinaryTree Int -> Bool
testSize tree = sizeTree tree == (length $ collapseTree tree)

--- testing that result tree is sorted.
testInsertOrder :: Int -> BinaryTree Int -> Bool
testInsertOrder n tree = (isSorted $ collapseTree $ insertTree n tree)


--- testing that if element is not already inside tree, then length should be greater
-- by 1, else they should be same size.
testInsertSize :: Int -> BinaryTree Int -> Bool
testInsertSize n oldTree
    | occurs n oldTree = (sizeTree newTree) ==  (sizeTree oldTree)
    | otherwise        = (sizeTree newTree) == (sizeTree oldTree + 1)
    where newTree = insertTree n oldTree


testOccurs :: Int -> BinaryTree Int -> Bool
testOccurs n tree = (elem n (collapseTree tree)) == (occurs n tree)


