import Prelude hiding (either)
import Test.QuickCheck
import Test.QuickCheck.Property
import Control.Monad

-- 14.2 RECURSIVE ALGEBRAIC TYPES ----------------------------------------------------

-- recursive types examples


data NTree = NilT | NodeT Integer NTree NTree deriving (Eq, Show)

tree1, tree2 :: NTree
tree1 = NodeT 10 NilT NilT
tree2 = NodeT 17 (NodeT 14 NilT NilT) (NodeT 20 NilT NilT)
tree3 = NodeT 3 (NodeT 4 NilT NilT) NilT

sumNTree :: NTree -> Integer
sumNTree NilT = 0
sumNTree (NodeT n t1 t2) = n + sumNTree t1 + sumNTree t2

sizeNTree :: NTree -> Integer
sizeNTree NilT = 0
sizeNTree (NodeT n t1 t2) = 1 + sizeNTree t1 + sizeNTree t2

depthNTree :: NTree -> Integer
depthNTree NilT = 0
depthNTree (NodeT n t1 t2) = 1 + max (depthNTree t1) (depthNTree t2)

-- note num times a number p occurs in tree
occurs :: NTree -> Integer -> Integer
occurs NilT p = 0
occurs (NodeT n t1 t2) p
    | n == p    = 1 + occurs t1 p + occurs t2 p
    | otherwise = occurs t1 p + occurs t2 p






----------------------------------------------------------------------------------


data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr deriving (Eq, Show)

expr1, expr2, expr3, expr4, expr5, expr6 :: Expr
expr1 = Lit 2                             -- 2
expr2 = Add (Lit 2) (Lit 3)               -- 2 + 3
expr3 = Add (Sub (Lit 3) (Lit 1)) (Lit 3) -- (3-1) + 3
expr4 = Add (Add (Lit 2) (Lit 3)) (Lit 4)
expr5 = Add expr4 (Lit 5)
expr6 = Add (Add (Sub (Lit 2) (Add (Add (Lit 6) (Lit 7)) (Lit 8))) (Lit 4)) (Lit 5)

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)


-- Rearranging expressions
{-
(2 + 3) + 4               2 + (3 + 4)
((2 + 3) + 4) + 5         2 + (3 + (4 + 5))
((2-((6+7)+8))+4)5        (2 - (6+(7+8))) + (4+5)
-}

assoc :: Expr -> Expr
assoc (Add (Add e1 e2) e3) = assoc (Add e1 (Add e2 e3))
assoc (Add e1 e2) = Add (assoc e1) (assoc e2)
assoc (Sub e1 e2) = Sub (assoc e1) (assoc e2)
assoc (Lit n) = Lit n


showExpr :: Expr -> String
showExpr expr = sho expr 0

sho :: Expr -> Int -> String
sho (Lit n) c = show n
sho (Add (Add e1 e2) e3) c -- (10 + 9) + 1
    | c == 0 =  main1
    | otherwise = applyOuterParens main1
    where main1 = "(" ++ sho e1 (c+1) ++ " + " ++ sho e2 (c+1) ++ ") + " ++ sho e3 (c+1)
sho (Add e1 (Add e2 e3)) c -- 10 + (9+1)
    | c == 0 = main2
    | otherwise = applyOuterParens main2
    where main2 = sho e1 (c+1) ++ " + (" ++ sho e2 (c+1) ++ " + " ++ sho e3 (c+1) ++ ")"

sho (Sub (Sub e1 e2) e3) c -- (5-4) - 6
    | c == 0 =  main3
    | otherwise = applyOuterParens main3
    where main3 = "(" ++ sho e1 (c+1) ++ " - " ++ sho e2 (c+1) ++ ") - " ++ sho e3 (c+1)
sho (Sub e1 (Sub e2 e3)) c -- 5 - (4-6)
    | c == 0 =  main4
    | otherwise = applyOuterParens main4
    where main4 = sho e1 (c+1) ++ " - (" ++ sho e2 (c+1) ++ " - " ++ sho e3 (c+1) ++ ")"

sho (Add (Sub e1 e2) e3) c -- (3-1) + 4
    | c == 0 = main5
    | otherwise = applyOuterParens main5
    where main5 = "(" ++ sho e1 (c+1) ++ " - " ++ sho e2 (c+1) ++ ") + " ++ sho e3 (c+1)
sho (Sub e1 (Add e2 e3)) c -- 3 - (1+4)
    | c == 0 = main6
    | otherwise = applyOuterParens main6
    where main6 = sho e1 (c+1) ++ " - (" ++ sho e2 (c+1) ++ " + " ++ sho e3 (c+1) ++ ")"
sho (Sub (Add e1 e2) e3) c -- (3+1) - 4
    | c == 0 = main7
    | otherwise = applyOuterParens main7
    where main7 = "(" ++ sho e1 (c+1) ++ " + " ++ sho e2 (c+1) ++ ") - " ++ sho e3 (c+1)
sho (Add e1 (Sub e2 e3)) c -- 3 + (1 - 4)
    | c == 0 = main8
    | otherwise = applyOuterParens main8
    where main8 = sho e1 (c+1) ++ " + (" ++ sho e2 (c+1) ++ " - " ++ sho e3 (c+1) ++ ")"

sho (Add e1 e2) c = "(" ++ sho e1 (c+1) ++ " + " ++ sho e2 (c+1) ++ ")"
sho (Sub e1 e2) c = "(" ++ sho e1 (c+1) ++ " - " ++ sho e2 (c+1) ++ ")"

applyOuterParens :: String -> String
applyOuterParens s = "(" ++ s ++ ")"

{- HELP how to eliminate boilerplate code in sho function?
applySho expr count exprStr
    | count == 0 = exprStr
    | otherwise = applyOuterParents exprStr
    where applyOuterParens s = "(" ++ s ++ ")"-}

{-
data Expr = Lit Integer
            | Expr :+: Expr
            | Expr :-: Expr-}






-- Mutual recursive types ----------------------------------------------------------
type Name' = String
type Address = String

data Person = Adult Name' Address Bio
            | Child Name'
            deriving (Eq, Show)

data Bio    = Parent String [Person]
            | NonParent String
            deriving (Eq, Show)

showPerson :: Person -> String
showPerson (Adult n a b) = show n ++ show a ++ showBio b

showBio :: Bio -> String
showBio (Parent st ps) = st ++ concat (map showPerson ps)










-- 14.3 POLYMORPHIC ALGEBRAIC TYPES -------------------------------------------------

{-
NOTE polymorphic types contain types a,b and etc
-}

-- EXAMPLE 1

data Pairs a = Pr a a deriving (Eq, Show)


p1 :: Pairs Integer
p1 = Pr 2 3
p2 :: Pairs [Int]
p2 = Pr [] [3]
p3 :: Pairs [a]
p3 = Pr [] [] -- HELP why can't it take a string for (a)?


equalPair :: Eq a => Pairs a -> Bool
equalPair (Pr x y) = x == y




-- EXAMPLE 2
infixr 5 ::: -- HELP is this the fixity declaration?
data List a = NilL | a ::: (List a)
    deriving (Eq, Ord, Show, Read)

{-
NOTE
2+3 ::: 4+5 ::: NilL
5 ::: (9 ::: NilL)
-}




-- EXAMPLE 3

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show, Read)

t1 :: Tree Integer
t1 = Node 12 (Node 34 Nil Nil) (Node 3 (Node 17 Nil Nil) Nil)

depth :: Tree a -> Integer
depth Nil = 0
depth (Node n t1 t2) = 1 + max (depth t1) (depth t2)

collapse :: Tree a -> [a]
collapse Nil = []
collapse (Node n t1 t2) = collapse t1 ++ [n] ++ collapse t2

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Nil = Nil
mapTree f (Node n t1 t2) = Node (f n) (mapTree f t1) (mapTree f t2)




-- EXAMPLE 4
{-
data Either a b = Left a | Right b
-}
either1, either2 :: Either String Int
either1 = Left "Duke of Prunes"
either2 = Right 33312

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False


either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x) = f x
either f g (Right y) = g y

applyLeft :: (a -> c) -> Either a b -> c
applyLeft f (Left x) = f x
applyLeft f (Right _) = error "applyLeft applied to Right"













-- 14.4 ERROR HANDLING -----------------------------------------------------------


-- note type: transmitting an error
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe g Nothing = Nothing
mapMaybe g (Just x) = Just (g x)

errDiv :: Integer -> Integer -> Maybe Integer
errDiv n m
    | (m /= 0) = Just (n `div` m)
    | otherwise = Nothing


-- note type: trapping an error
maybe' :: b -> (a -> b) -> Maybe a -> b
maybe' n f Nothing = n
maybe' n f (Just x) = f x




{-
main = do
    print $ maybe' 56 (1+) (mapMaybe (*3) (errDiv 9 0))
    print $ maybe' 56 (1+) (mapMaybe (*3) (errDiv 9 1))
    print $ maybe' 56 (1+) (mapMaybe (*3) (Just 4))
-}






-- 14.5 DESIGN WITH ALGEBRAIC TYPES ------------------------------------------------

-- EXAMPLE PROBLEM 1 - edit distance - how many edit sequences to result in a
-- particular string?
{-
NOTe: types of edits:
1. convert character into another
2. copy a char without modifying it
3. delete one char
4. insert one char
5. delete (kill) to the end of the string

NOTE design stages
1. identify types of data involved
data Edit = ...

2. different sorts of data in each of the types. Each sort is the constructor
data Edit = Chagne .. | Copy .. | Deelte .. | Insert .. | Kill ..

3. types of args for each constructor (decide component of the constructor)
data Edit = Change Char | Copy | Delete | Insert Char | Kill
-}





{-
-- EXAMPLE PROBLEM 2 - Simulation - get input of customer arrivals (queues) and
give output their departures (decide how many bank clerks need to be working at
certain times of the day).

1. type of input message: Inmess.
At a given time
    => no one arrives (No)
    => someone arrives (Yes (arrival time of customer) (time to serve them))

    => data InputMsg = No | Yes Arrival Service
       type Arrival = Integer
       type Service = Integer

2.outmess - type of output messages.At a given time, either
    =>  no one leaves (None)
    => or a person is discharged (Discharge) which takes the time the customer
    waited and time of their arrival and time it took to serve them.

    data OutputMsg = None | Discharge Arrival Wait Service
    type Wait = Integer
-}



-- BACK TO EXAMPLE PROBLEM 1

data Edit = Change Char
          | Copy
          | Delete
          | Insert Char
          | Kill
          deriving (Eq, Show)

-- note finds lowest cost sequence of edits to take us from one string to another.
-- in general case, if first two chars of strnisg are equal, then Copy. Otherwise,
-- try all possibilities and choose the best of them.
transform :: String -> String -> [Edit]
transform [] [] = []
transform xs [] = [Kill] -- note to turn xs -> [] just kill it
transform [] ys = map Insert ys  -- note to turn [] -> ys insert ys.
transform (x:xs) (y:ys)
    | x == y = Copy : transform xs ys
    | otherwise = best [Delete   : transform xs (y:ys),
                        Insert y : transform (x:xs) ys,
                        Change y : transform xs ys]


best :: [[Edit]] -> [Edit]
best [es] = es
best (es : ess)
    | cost es <= cost b = es
    | otherwise = b
    where b = best ess

-- note cost is given by charging one for every operation except copy which means
-- leave unchanged
cost :: [Edit] -> Int
cost = length . filter (/= Copy)




-- note builds the new string from the old string using the operations in the list.
-- test edit [Insert 'c', Change 'h', Copy, Insert 'p', Copy, Kill] "fish" == chips
edit :: [Edit] -> String -> String
edit _ [] = []
edit [] xs = xs
edit (Insert c : rest) (x:xs) = c : edit rest (x:xs)
edit (Change c : rest) (x:xs) = c : edit rest xs
edit (Copy : rest) (x:xs) = x : edit rest xs
edit (Delete : rest) (x:xs) = edit rest xs
edit (Kill : rest) xs = []












-- 14.6 ALGEBRAIC TYPES AND TYPE CLASSES -------------------------------------------

{-data Vector = Vec Float Float deriving (Eq, Show)
data Point = Point Float Float deriving (Eq, Show)
data Figure = Line Point Point | Circle Point Float deriving (Eq, Show)

class Movable a where
    move      :: Vector -> a -> a
    reflectX  :: a -> a
    reflectY  :: a -> a
    rotate180 :: a -> a
    rotate180 = reflectX . reflectY

instance Movable Point where
    move (Vec v1 v2) (Point x y) = Point (x + v1) (y + v2)
    reflectX (Point x y) = Point x (-y)
    reflectY (Point x y) = Point (-x) y
    rotate180 (Point x y) = Point (-x) (-y) -- more efficient to override with this def.

instance Movable Figure where
    move v (Line p1 p2) = Line (move v p1) (move v p2)
    move v (Circle p r) = Circle (move v p) r

    reflectX (Line p1 p2) = Line (reflectX p1) (reflectX p2)
    reflectX (Circle p r) = Circle (reflectX p) r

    reflectY (Line p1 p2) = Line (reflectY p1) (reflectY p2)
    reflectY (Circle p r) = Circle (reflectY p) r


instance Movable a => Movable [a] where
    move v = map (move v) -- list of points/circles/lines
    reflectX = map reflectX -- list here
    reflectY = map reflectY -- list here-}







-- NOTE need to write arbitrary instancs if I use my own data type.
-- Look: https://www.stackage.org/lts-6.4/hoogle?q=Test.QuickCheck
instance Arbitrary Point where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Point x y)

instance Arbitrary Vector where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Vec x y)
{-
HELP HELP HELP -- to test Movable [a] instance
TODO do we need Arbitrary Figure?
instance Arbitrary Figure where
    arbitrary =
        frequency [ (1, liftM2 Line arbitrary arbitrary),
                    (2, liftM2 Point arbitrary arbitrary)]
-}


    {-oneof
        [return $ Line arbitrary arbitrary,
         return $ Point arbitrary arbitrary]-}


propMove :: Vector -> Point -> Bool
propMove (Vec vx vy) (Point px py) =
    move (Vec vx vy) (Point px py) == Point (vx + px) (vy + py)

------------------------------------------------------------------------------------
swapPoint :: Point -> Point
swapPoint (Point x y) = Point y x


-- swap . swap == id
propSwapPoint :: Point -> Bool
propSwapPoint point = (swapPoint . swapPoint) point == point

------------------------------------------------------------------------------------

-- reflectX . reflectX = id
propReflectX :: Point -> Bool
propReflectX point = (reflectX . reflectX) point == point

propReflectY :: Point -> Bool
propReflectY point = (reflectY . reflectY) point == point

propRotate :: Point -> Bool
propRotate point = (rotate180 . rotate180) point == point


point1 = Point 3 5
vector1 = Vec 15 2
point = Point 1 2
line = Line (Point 5 3) (Point 10 (-2))
circle = Circle (Point 5 (-1)) 8.367
{-
uncover
main = do
    quickCheckWith stdArgs {maxSuccess = 1000} propMove
    quickCheckWith stdArgs {maxSuccess = 500} propSwapPoint
    quickCheckWith stdArgs {maxSuccess = 500} propReflectX
    quickCheckWith stdArgs {maxSuccess = 500} propReflectY
    print $ move (Vec 13 4) [line, circle]
    -- HELP why won't it work to write point in this list? It is an instance-
    -- of Movable... ??
-}









-- Combining Named and Movable


data Vector = Vec Float Float deriving (Eq, Show)
data Point = Point Float Float deriving (Eq, Show)
data Figure = Line Point Point | Circle Point Float deriving (Eq, Show)

class Movable a where
    move      :: Vector -> a -> a
    reflectX  :: a -> a
    reflectY  :: a -> a
    rotate180 :: a -> a
    rotate180 = reflectX . reflectY

instance Movable Point where
    move (Vec v1 v2) (Point x y) = Point (x + v1) (y + v2)
    reflectX (Point x y) = Point x (-y)
    reflectY (Point x y) = Point (-x) y
    rotate180 (Point x y) = Point (-x) (-y) -- more efficient to override with this def.

instance Movable Figure where
    move v (Line p1 p2) = Line (move v p1) (move v p2)
    move v (Circle p r) = Circle (move v p) r

    reflectX (Line p1 p2) = Line (reflectX p1) (reflectX p2)
    reflectX (Circle p r) = Circle (reflectX p) r

    reflectY (Line p1 p2) = Line (reflectY p1) (reflectY p2)
    reflectY (Circle p r) = Circle (reflectY p) r


instance Movable a => Movable [a] where
    move v = map (move v) -- list of points/circles/lines
    reflectX = map reflectX -- list here
    reflectY = map reflectY -- list here

----------------------------------------------------------------

data Name a = Pair a String deriving (Eq, Show)

class Named a where
    lookName :: a -> String
    putName  :: String -> a -> a

instance Named (Name a) where
    lookName (Pair obj nm) = nm
    putName nm (Pair obj _) = Pair obj nm


mapName :: (a -> b) -> Name a -> Name b
mapName f (Pair obj nm) = Pair (f obj) nm

-- note adding names to the movable objects.
instance Movable a => Movable (Name a) where
    move v = mapName (move v)  -- the name pair arg here
    reflectX = mapName reflectX -- the name pair arg here
    reflectY = mapName reflectY -- the name pair arg here



--------------------------------------------------------------
-- NOTE important even without these below definitions, the main tests still work.
-- HELP how do these classes contribute? How to test them? What do they do?
class (Movable b, Named b) => NamedMovable b

instance Movable a => NamedMovable (Name a)



name :: Name Int
name = Pair 423 "four-twenty-three"
exam1 = Pair (Point 1.1 2.3) "Dweezil"
{-
uncover
main = do
    print $ lookName name
    print $ putName "four hundred and twenty three" name
    print $ lookName exam1 == "Dweezil"
    print $ move (Vec 13 7) exam1
    print $ rotate180 exam1
    print $ reflectX exam1-}











-- 14.7 REASONING ABOUT ALGEBRAIC TYPES -------------------------------------------

-- EXAMPLE PROOF 1
{-
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Proposition: map f (collapse tr) = collapse (mapTree f tr)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

data Tree a = Nil | Node a (Tree a) (Tree a)

map f [] = []
map f (x:xs) = f x : map f xs

mapTree f Nil = Nil
mapTree f (Node x t1 t2) = Node (f x) (mapTree f t1) (mapTree f t2)

collapse Nil = []
collapse (Node x t1 t2) = collapse t1 ++ [x] ++ collapse t2



^^^^^^^^^^^^^
1. BASE CASE:
^^^^^^^^^^^^^

LEFT
map f (collapse Nil)
= map f []
= []

RIGHT
collapse (mapTree f Nil)
= collapse Nil
= []



^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
2. ASSUME INDUCTION HYPOTHESIS
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

map f (collapse t1) = collapse (mapTree f t1)
map f (collapse t2) = collapse (mapTree f t2)



^^^^^^^^^^^^^
3. INDUCTION
^^^^^^^^^^^^^

LEFT
map f (collapse (Node x t1 t2))
= map f (collapse t1 ++ [x] ++ collapse t2)
= map f (collapse t1) ++ [f x] ++ map f (collapse t2)
= collapse (mapTree f t1) ++ [f x] ++ collapse (mapTree f t2)  equals ind hypothesis


RIGHT
collapse (mapTree f (Node x t1 t2))
= collapse (Node (f x) (mapTree f t1) (mapTree f t2))
= collapse (mapTree f t1) ++ [f x] ++ collapse (mapTree f t2)
-}






-- EXAMPLE PROOF 2
{-

-- note if the maybe is Nothing then return default value b. Else if just, then,
-- apply the functo inside the just and return the content.
maybe :: b -> (a -> b) -> Maybe a -> b

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Proposition: maybe 2 abs x >= 0
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


^^^^^^^^^^^^^^^^^^^^^
1. PROVE OVER NOTHING
^^^^^^^^^^^^^^^^^^^^^

maybe 2 abs x
= maybe 2 abs Nothing
= 2 >= 0


^^^^^^^^^^^^^^^^^^
2. PROVE OVER JUST
^^^^^^^^^^^^^^^^^^

maybe 2 abs x
= maybe 2 abs (Just y)
= abs y >= 0

-}





-- EXAMPLE PROOF 3

{-
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Proposition: eval (assoc ex) = eval ex
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


assoc :: Expr -> Expr
assoc (Add (Add e1 e2) e3) = assoc (Add e1 (Add e2 e3))
assoc (Add e1 e2) = Add (assoc e1) (assoc e2)
assoc (Sub e1 e2) = Sub (assoc e1) (assoc e2)
assoc (Lit n) = Lit n


^^^^^^^
CASE 1    Lit
^^^^^^^

LEFT

eval (assoc (Lit n))
= eval (Lit n)
= n


RIGHT

eval (Lit n)
= n


^^^^^^^
CASE 2    Add: prove: eval (assoc (Add e1 e2)) = eval (Add e1 e2)
^^^^^^^

assoc.2
eval (assoc (Add e1 e2)) = eval (Add e1 e2)

assoc.1
eval (assoc (Add (Add f1 f2) e2)))
= eval (assoc (Add f1 (Add f2 e2)))
note since f1 contains fewer Adds now
= eval (Add f1 (Add f2 e2))
= eval (Add (Add f1 f2) e2)



^^^^^^^
CASE 3    Sub
^^^^^^^

-}

-- data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr
instance Arbitrary Expr where
    arbitrary = frequency
        [(1, liftM Lit arbitrary),
         (2, liftM2 Add arbitrary arbitrary),
         (2, liftM2 Sub arbitrary arbitrary)]


    --sized arbExpr
{-

arbExpr 0 = liftM Lit arbitrary
arbExpr n =
    frequency [ (1, liftM Lit arbitrary),
                (4, liftM2 Add (arbExpr (n `div` 2)), (arbExpr (n `div` 2))),
                (4, liftM2 Sub (arbExpr (n `div` 2)), (arbExpr (n `div` 2)))]
-}


propAssoc :: Expr -> Bool
propAssoc expr = eval expr == eval (assoc expr)

propDepth :: NTree -> Bool
propDepth t = sizeNTree t < 2^(depthNTree t)

propCollapse :: Eq b => (a -> b) -> Tree a -> Bool
propCollapse f t = map f (collapse t) == collapse (mapTree f t)