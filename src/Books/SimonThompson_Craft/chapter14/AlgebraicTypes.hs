import Prelude hiding (either)
import Test.QuickCheck
import Test.QuickCheck.Property

-- 14.2 RECURSIVE ALGEBRAIC TYPES ----------------------------------------------------

-- recursive types examples


data NTree = NilT | NodeNTree Integer NTree NTree deriving (Eq, Show)

tree1, tree2 :: NTree
tree1 = NodeNTree 10 NilT NilT
tree2 = NodeNTree 17 (NodeNTree 14 NilT NilT) (NodeNTree 20 NilT NilT)
tree3 = NodeNTree 3 (NodeNTree 4 NilT NilT) NilT

sumTree :: NTree -> Integer
sumTree NilT = 0
sumTree (NodeNTree n t1 t2) = n + sumTree t1 + sumTree t2

depthNTree :: NTree -> Integer
depthNTree NilT = 0
depthNTree (NodeNTree n t1 t2) = 1 + max (depthNTree t1) (depthNTree t2)

-- note num times a number p occurs in tree
occurs :: NTree -> Integer -> Integer
occurs NilT p = 0
occurs (NodeNTree n t1 t2) p
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
type Name = String
type Address = String

data Person = Adult Name Address Bio
            | Child Name
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



-- HELP HELP HELP TODO why do these properties fail?

-- NOTE properties of transform:
-- PROP 1: cost if its operations should be no larger than cost of building target
-- string letter by letter and then killing oridinal string (cost of length ys +1)
propTransformLength :: String -> String -> Property
propTransformLength xs ys = length (xs ++ ys) <= 15 ==> -- constrained < 15 for efficiency
    cost (transform xs ys) <= length ys + 1

-- PROP 2: sequence of edits resulting should indeed take the string xs to ys
-- when it is applied
propTransform xs ys = length (xs ++ ys) <= 15 ==>
    edit (transform xs ys) xs == ys

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
