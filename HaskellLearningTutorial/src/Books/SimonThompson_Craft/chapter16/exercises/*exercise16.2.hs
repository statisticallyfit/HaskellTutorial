

-- Part 1 - define (==) for this type of implementation of store
type Var = Char
newtype Store = Store [(Integer, Var)]

-- note checks if the integers in one list are equal to the integers of the other list.
instance Eq Store where
    (Store s1) == (Store s2) = valsCrossEqual
        where zipped = zip s1 s2
              valsCrossEqual = and $ map (\(p1,p2) -> fst p1 == fst p2) zipped

-- HELP why showsPrec and not show?
instance Show Store where
    showsPrec n (Store s) = showsPrec n s



sto1 = Store [(10,'a'), (2,'b'), (2,'b'), (5,'c'), (6,'d'), (0,'j'), (99,'k'),(101,'k')]
sto2 = Store [(10,'z'), (3,'z'), (2,'p'), (8,'m'), (7,'n'), (5,'a'), (3,'b')]
sto3 = Store [(10,'z'), (2,'m'), (2,'b'), (5,'c'), (6,'f'), (0,'g'), (99,'e'), (101,'a')]
-----------------------------------------------------------------------------------------



-- Part 2 - define (==) for the function implementation of store

newtype Stock = Stock (Var -> Integer)

initial :: Stock
initial = Stock (\v -> 0)

value :: Stock -> Var -> Integer
value (Stock s) v = s v

-- HELP understand better.
update :: Stock -> Var -> Integer -> Stock
update (Stock s) v n = Stock (\w -> if w == v then n else (s w))



-- HELP - how to define the Eq which says two stocks are equal if they
-- result in same int?

{-
instance Show (Var -> Integer) where
    show f = "\\" ++ show v ++ " -> " ++ show int
        where (\v -> int) = f


instance Show (Stock Var) where
    show (Stock sto var) = show (sto var)
-}

{-
HELP
instance Eq Stock where
    (Stock s1) == (Stock s2) =
-}



stock1 :: Stock
stock1 = Stock (\v -> fst $ head $ filter (\(n,c) -> c == v) intVarMap)


-- the map that is used to return the integer for a particular var using stock1
intVarMap :: [(Integer, Var)]
intVarMap = [(1,'a'),(4,'b'),(2,'g'),(10,'u'),(3,'z'),(9,'t'),(33,'j'),(7,'k'),(8,'o'),
             (5,'c'),(4,'d'),(11,'e'),(12,'f'),(13,'n'),(20,'h'),(14,'i'),(15,'m')]
