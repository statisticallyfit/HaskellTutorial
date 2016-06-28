import Test.QuickCheck


type Var = Char -- HELP what is Var supposed to be?
newtype Store = Store [(Integer, Var)] -- deriving (Eq, Show)

initial :: Store
initial = Store []

value :: Store -> Var -> Integer
value (Store []) v = 0
value (Store ((n,w) : rest)) v
    | v == w  = n
    | otherwise = value (Store rest) v

-- note put the new pair at the font.
update :: Store -> Var -> Integer -> Store
update (Store sto) v n = Store ((n,v):sto)


-- exercise 5 --------------
-- note need to be given a store as an arg, right? HELP modified this type from the book
-- from page 406 (real)
setAll :: Integer -> Store -> Store
setAll newN (Store sto) = Store (map (\(oldN, char) -> (newN, char)) sto)
----------------------------

instance Show Store where
    showsPrec n (Store s) = showsPrec n s


sto1 = Store [(10,'a'), (2,'b'), (2,'b'), (0,'j'), (99,'k'),(101,'k')]
sto2 = Store [(10,'z'), (3,'z'), (2,'p'), (8,'m'), (7,'n'), (5,'a'), (3,'b')]
sto3 = Store [(10,'z'), (2,'m'), (2,'b'), (0,'g'), (99,'e'), (101,'a')]


------------------------------------------------------------------

newtype Stock = Stock (Var -> Integer)

initial' :: Stock
initial' = Stock (\v -> 0)

value' :: Stock -> Var -> Integer
value' (Stock s) v = s v

-- HELP understand better.
update' :: Stock -> Var -> Integer -> Stock
update' (Stock s) v n = Stock (\w -> if w == v then n else (s w))




-- exercise 5 ------------
-- HELP TODO how to define setAll with this Stock implementation?
--------------------------



stock1 :: Stock
stock1 = Stock (\v -> fst $ head $ filter (\(n,c) -> c == v) intVarMap)

-- the map that is used to return the integer for a particular var using stock1
intVarMap :: [(Integer, Var)]
intVarMap = [(1,'a'),(4,'b'),(2,'g'),(10,'u'),(3,'z'), (9,'t'),(33,'j'),(7,'k'),
             (8,'o'), (5,'c'), (4,'d'),(11,'e'),(12, 'f'),(13,'n'), (20,'h'),
             (14,'i'),(15,'m')]








-- HELP why not working when I run them? ------------------------------------------------

-- note once we update, when we look up value, we expect to see the new value.
propUpdate1 :: Char -> Integer -> Stock -> Bool
propUpdate1 c n sto = value' (update' sto c n) c == n

-- note after an update, if we look up value of another variable, its value should
-- be same as before
-- HELP TODO understand better.
propUpdate2 :: Char -> Char -> Integer -> Stock -> Bool
propUpdate2 c1 c2 n sto = value' (update' sto c2 n) c1 == value' sto c1 || c1 == c2
-- note this fails when c1 == c2
--propUpdate2 c1 c2 n sto = value (update sto c2 n) c1 == value sto c1