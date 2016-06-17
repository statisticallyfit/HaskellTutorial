

-- 14.2 RECURSIVE ALGEBRAIC TYPES ----------------------------------------------------

-- recursive types examples

data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr deriving (Eq, Show)

expr1, expr2, expr3 :: Expr
expr1 = Lit 2                             -- 2
expr2 = Add (Lit 2) (Lit 3)               -- 2 + 3
expr3 = Add (Sub (Lit 3) (Lit 1)) (Lit 3) -- (3-1) + 3


eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)






data NTree = NilT | Node Integer NTree NTree deriving (Eq, Show)

tree1, tree2 :: NTree
tree1 = Node 10 NilT NilT
tree2 = Node 17 (Node 14 NilT NilT) (Node 20 NilT NilT)
tree3 = Node 3 (Node 4 NilT NilT) NilT

sumTree :: NTree -> Integer
sumTree NilT = 0
sumTree (Node n t1 t2) = n + sumTree t1 + sumTree t2

depth :: NTree -> Integer
depth NilT = 0
depth (Node n t1 t2) = 1 + max (depth t1) (depth t2)

-- note num times a number p occurs in tree
occurs :: NTree -> Integer -> Integer
occurs NilT p = 0
occurs (Node n t1 t2) p
    | n == p    = 1 + occurs t1 p + occurs t2 p
    | otherwise = occurs t1 p + occurs t2 p





-- Rearranging expressions
{-
(2 + 3) + 4               2 + (3 + 4)
((2 + 3) + 4) + 5         2 + (3 + (4 + 5))
((2-((6+7)+8))+4)5        (2 - (6+(7+8))) + (4+5)
-}