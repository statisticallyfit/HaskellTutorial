import Data.Char

-- Part 1 - define Store with Maybe
type Var = Char
newtype Store = Store [(Integer, Maybe Var)]

initial :: Store
initial = Store []

value :: Store -> Var -> Maybe Integer
value (Store []) v = Nothing
value (Store ((n,w) : rest)) v
    | Just v == w  = Just n
    | otherwise = value (Store rest) v

-- note put the new pair at the font.
update :: Store -> Var -> Integer -> Store
update (Store sto) v n = Store ((n, Just v):sto)



sto1 = Store [(10,Just 'a'), (2,Just 'b'), (2,Just 'b'), (0,Just 'j'),
              (99,Just 'k'),(101,Just 'k')]
sto2 = Store [(10,Just 'z'), (3,Just 'z'), (2,Just 'p'), (8,Just 'm'),
              (7,Just 'n'), (5,Just 'a'), (3,Just 'b')]
sto3 = Store [(10,Just 'z'), (2,Just 'm'), (2,Just 'b'), (0,Just 'g'),
              (99,Just 'e'), (101,Just 'a')]

-----------------------------------------------------------------------------------------



-- Part 2 - define Stock (func) with Maybe

newtype Stock = Stock (Var -> Maybe Integer)

initial' :: Stock
initial' = Stock (\v -> Nothing)

value' :: Stock -> Var -> Maybe Integer
value' (Stock s) v = s v

-- HELP understand better.
update' :: Stock -> Var -> Integer -> Stock
update' (Stock s) v n = Stock (\w -> Just (toInteger $ ord w))
-- if v == w then Just n else (s w)



stock1 :: Stock
stock1 = Stock (\v -> fst $ head $ filter (\(n,c) -> c == v) intVarMap)


-- the map that is used to return the integer for a particular var using stock1
intVarMap :: [(Maybe Integer, Var)]
intVarMap = [(Just 1,'a'),(Just 4,'b'),(Just 2,'g'),(Just 10,'u'),(Just 3,'z'),
             (Just 9,'t'),(Just 33,'j'),(Just 7,'k'),(Just 8,'o'), (Just 5,'c'),
             (Just 4,'d'),(Just 11,'e'),(Just 12, 'f'),(Just 13,'n'),
             (Just 20,'h'),(Just 14,'i'),(Just 15,'m')]
