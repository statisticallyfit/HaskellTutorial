import Test.QuickCheck
import Control.Monad hiding (join)

{-
NOTE building calculator for numerical expressions.
Sets values of variables.
Store: current variable values.

Abstract data type (ADT): limited interface by means of specified set of
operatoins.
Example: we hide the information about how type Store is implemented.
Also note: if we export a type like this => Data without (..) then constructors are
not exported so we can only operate on type through functions defined on it.
Example module Store (Store, initial, value, update) where
Here, constructors of Store are not available so we can only operate on Store using its
functions.

-}


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


instance Eq Store where
    (Store s1) == (Store s2) = (s1 == s2)

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




------------------------------------------------------------------


instance Arbitrary Store where
    arbitrary = do
        x <- arbitrary --integer
        y <- arbitrary -- char (var)
        return (Store [(x,y)])



-- Testing ADTs
propInitial :: Char -> Bool
propInitial ch = value initial ch == 0

-- note once we update, when we look up value, we expect to see the new value.
propUpdate1 :: Char -> Integer -> Store -> Bool
propUpdate1 c n sto = value (update sto c n) c == n

-- note after an update, if we look up value of another variable, its value should
-- be same as before
-- HELP TODO understand better.
propUpdate2 :: Char -> Char -> Integer -> Store -> Bool
propUpdate2 c1 c2 n sto = value (update sto c2 n) c1 == value sto c1 || c1 == c2
-- note this fails when c1 == c2
--propUpdate2 c1 c2 n sto = value (update sto c2 n) c1 == value sto c1
