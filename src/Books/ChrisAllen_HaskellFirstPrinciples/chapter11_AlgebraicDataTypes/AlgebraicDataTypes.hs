

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

-- note cardinality of newtype is same as that of type it contains
-- newtype is special: after comile time, Goat becomes identical to Int. 
newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Int -> Bool
tooManyGoats (Goats n) = n > 42