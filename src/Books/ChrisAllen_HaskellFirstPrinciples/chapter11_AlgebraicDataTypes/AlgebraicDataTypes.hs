{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Int


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
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)


--- > example 3 values in sum. 
type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWoold = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)
data Animal = Cow CowInfo
            | Pig PigInfo
            | Sheep SheepInfo
            deriving (Eq, Show)
-- alternatively
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)
