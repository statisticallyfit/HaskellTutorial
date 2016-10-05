module ExprTreeOps where

import Test.QuickCheck -- (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad hiding (join)
import Data.Char
import Numeric
import Data.Maybe


data Function
    = Sin Expr | Cos Expr | Tan Expr |
      Csc Expr | Sec Expr | Cot Expr |
      Arcsin Expr | Arccos Expr | Arctan Expr |
      Arccsc Expr | Arcsec Expr | Arccot Expr |
      Ln Expr | E Expr | Log Expr Expr -- first expr is base
    deriving (Eq)


data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
    | Pow Expr Expr | Neg Expr | Num Int  {-Var Expr-} | X | Y | F Function
    deriving (Eq)

type Coeff = Int
type Exps = Int

data ExprHolder = Polynomial [Coeff] | Trig [(Coeff, Exps, Expr)] | InvTrig [(Coeff, Exps, Expr)] |
    Hyperbolic [(Coeff, Exps, Expr)] | Logarithmic [(Coeff, Exps, Expr)]
    deriving (Eq, Show)

data Tree a = Empty | Leaf a | Node String (Tree a) (Tree a) deriving (Eq)

-- TODO for fractional int dividing
-- a = fst . head $ readFloat "0.75" :: Rational

instance Show Expr where
    show X = "x"
    show Y = "y"
    show (F func) = show func
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2) = show e1 ++ " - " ++ show e2
    show (Mul (Num n) (Mul (Num m) (Mul (Num p) rest)))
        = "(" ++ show n ++ ")(" ++ show m ++ ")(" ++ show p ++ ")" ++ show rest
    show (Mul (Num n) (Mul (Num m) rest)) = "(" ++ show n ++ ")(" ++ show m ++ ")" ++ show rest
    show (Mul e1 e2) = show e1 ++ show e2
    show (Div e1 e2) = show e1 ++ " / " ++ show e2
    show (Pow e1 e2) = show e1 ++ "^" ++ show e2
    show (Neg e) = "-(" ++ show e ++ ")"
    show (Num n) = show n

instance Show Function where
    show (Sin e) = "sin(" ++ show e ++ ")"
    show (Cos e) = "cos(" ++ show e ++ ")"
    show (Tan e) = "tan(" ++ show e ++ ")"
    show (Csc e) = "csc(" ++ show e ++ ")"
    show (Sec e) = "sec(" ++ show e ++ ")"
    show (Cot e) = "cot(" ++ show e ++ ")"
    show (Arcsin e) = "arcsin(" ++ show e ++ ")"
    show (Arccos e) = "arccos(" ++ show e ++ ")"
    show (Arctan e) = "arctan(" ++ show e ++ ")"
    show (Arccsc e) = "arccsc(" ++ show e ++ ")"
    show (Arcsec e) = "arcsec(" ++ show e ++ ")"
    show (Arccot e) = "arccot(" ++ show e ++ ")"
    show (Ln e) = "ln(" ++ show e ++ ")"
    show (Log b a) = "log" ++ show b ++ "(" ++ show a ++ ")"

{-

e3 should look like:
(-)
 |
 `-- (+)
 |    |
 |    `-- (*)
 |    |    |
 |    |    `-- 4
 |    |    `-- x
 |    |
 |    `-- (*)
 |         |
 |         `-- 3
 |         `-- (^)
 |              |
 |              `-- x
 |              `-- 5
 |
 `-- sin(*) ...

instance Show a => Show (Tree a) where
    show theTree = draw 1 "\n     " "\n" "\n" theTree
        where
        draw _ _ _ _ Empty = "_"
        draw _ _ _ _ (Leaf n) = show n
        draw c i lb rb (Node op left right)
            | isNode left && isNode right
                = "(" ++ op ++ ")" ++ lb' ++ draw c' i' lb' rb' left ++
                    lb' ++ draw c' i' lb' rb' right
            | isNode left
                = "(" ++ op ++ ")" ++ lb' ++ draw c' i' lb' rb' left ++
                    rb' ++ draw c' i' lb' rb' right
            | isNode right
                = rb' ++ draw c' i' lb' rb' left ++
                    "(" ++ op ++ ")" ++ lb' ++ draw c' i' lb' rb' right
            where
            i' = i ++ "     "
            c' = c + 1
            lb' = lb ++ " |\n `-- "
            rb' = " |\n"  ++ i'
-}


instance Show a => Show (Tree a) where
    show tree = draw 1 "\n" tree
        where
        draw _ _ Empty = "Empty"
        draw _ _ (Leaf n) = "Leaf " ++ show n
        draw count indent (Node n left right)
            = "Node " ++ (show n) ++ indent' ++ draw count' indent' left
            ++ indent' ++ draw count' indent' right
            where
            indent' = indent ++ "    "
            count' = count + 1



infixl 6 .+
infixl 6 .-
infixr 7 .*  -- NOTE made this right associative so tree would lean to the left (go right down)
infixl 7 ./
infixl 8 .^

(.+), (.-), (.*), (./), (.^) :: Expr -> Expr -> Expr
(.+) = Add
(.-) = Sub
(.*) = Mul
(./) = Div
(.^) = Pow

---------------------------------------------------------------------------------------------

e1 :: Expr
e1 = Num(4) .* X .* (F (Sin X)) .* Num(2) .* (F (Cos X)) .* Num(5) .* Num(2) .* Num(3) .* (F (Tan X))
e2 = e1 .+ Num(2) .* X .+ Num(7) .* X
e3 = Num(4) .* X .+ Num(3) .* X .^ Num(5) .- (F (Sin (Num(3) .+ X)))
e4 = Neg(F (Sin X)) .* Neg(Num(2))
e5 = Neg (Num(4) .* X .+ Num(2) .* Y)

-- TODO test percolate minus cases
t1 :: Tree Expr
t1 = Node "-" (Node "-" (Leaf $ Num 1) (Leaf $ Num 4)) (Leaf $ Num (-10))
t2 = Node "-" Empty (Node "*" (Leaf X) (Leaf $ Num 3))
t3 = Node "-" (Leaf $ Num 4) Empty


{-
NOTE:
TODO be able to fold over tree and simplify constants but also remember order that they
were in the tree!
Then remember also where they go at the end!
TODO get infinite decimal to fraction converter.
-}

-- TODO idea: make array holding coefficients of powers
-- so that Array (2, 0, 1, 5) means 5x^3 + x^2 + 2
-- then we can add/sub/mul/div/pow.
-- Once we simplify the expression this way (single elements only)
-- then we can use the tree to simplify things like x^2 * sin^3(2x)
-- by providing a ~= operator that is true if the structure is the same.
{-
data Function
    = Sin Expr | Cos Expr | Tan Expr |
      Csc Expr | Sec Expr | Cot Expr |
      Arcsin Expr | Arccos Expr | Arctan Expr |
      Arccsc Expr | Arcsec Expr | Arccot Expr |
      Ln Expr | E Expr | Log Expr Expr -- first expr is base
    deriving (Eq)


data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
    | Pow Expr Expr | Neg Expr | Num Int  -}
{-Var Expr-}{-
 | X | Y | F Function
    deriving (Eq)
-}

exprHolder :: Expr -> ExprHolder
exprHolder
exprHolder (Add e1 e2) =
exprHolder (Sub e1 e2) =

-- NOTE rules:
-- POlynomial: [1, 0, 2, 7, -4] represents 1 + x^2 + 7x^3 - 4x^4
-- Trig: [(0,1), (1,1), (4,1), (1,1), (2,4), (3, 5)]
--      represents (cos x + 4tan x + csc x + 2sec^4 x  + 3cot^5 x
--      where x = any expression and note (0, 1) = 0 always and (1,0) gets error
holderExpr :: ExprHolder -> Expr
holderExpr (Polynomial cs) = reverse $ map simplify $ zipWith (.*) cs (zipWith (.^) xs es )
    where xs = replicate (length cs) X
          es = map Num [0 .. (length cs - 1)]
-- TODO inside function is holder -- find way to deal with that.
holderExpr (Trig (cs, es, u)) =
    where fs = [F $ Sin u, F $ Cos u, F $ Tan u, F $ Csc u, F $ Sec u, F $ Cot u]


-- TODO types wrong
--- insdie the function is a type expression holder that is converted to expression.
{-holderExprFunctions :: Expr -> Expr
holderExprFunctions (F (Sin h)) = -}


-- puts an expression inside a function
push :: Expr -> Function -> Expr
push u (F (Sin v)) = F $ Sin $ simplify e
push u (F (Cos v)) = F $ Cos $ simplify e
push u (F (Tan v)) = F $ Tan $ simplify e
push u (F (Csc v)) = F $ Csc $ simplify e
push u (F (Sec v)) = F $ Sec $ simplify e
push u (F (Cot v)) = F $ Cot $ simplify e
push u (F (Arcsin v)) = F $ Arcsin $ simplify e
push u (F (Arccos v)) = F $ Arccos $ simplify e
push u (F (Arctan v)) = F $ Arctan $ simplify e
push u (F (Arccsc v)) = F $ Arccsc $ simplify e
push u (F (Arcsec v)) = F $ Arcsec $ simplify e
push u (F (Arccot v)) = F $ Arccot $ simplify e


simplify :: Expr -> Expr
simplify X = X
simplify Y = Y
simplify (Num n) = Num n
simplify (F (Sin e)) = F $ Sin $ simplify e
simplify (F (Cos e)) = F $ Cos $ simplify e
simplify (F (Tan e)) = F $ Tan $ simplify e
simplify (F (Csc e)) = F $ Csc $ simplify e
simplify (F (Sec e)) = F $ Sec $ simplify e
simplify (F (Cot e)) = F $ Cot $ simplify e
simplify (F (Arcsin e)) = F $ Arcsin $ simplify e
simplify (F (Arccos e)) = F $ Arccos $ simplify e
simplify (F (Arctan e)) = F $ Arctan $ simplify e
simplify (F (Arccsc e)) = F $ Arccsc $ simplify e
simplify (F (Arcsec e)) = F $ Arcsec $ simplify e
simplify (F (Arccot e)) = F $ Arccot $ simplify e
simplify (Pow e (Num 0)) = Num 1
simplify (Pow e (Num 1)) = simplify e
simplify (Mul (Num 1) e2) = simplify e2
simplify (Neg e) = Neg $ simplify e
simplify (Add e1 e2) = simplify e1 .+ simplify e2
simplify (Sub e1 e2) = simplify e1 .- simplify e2
simplify (Mul e1 e2) = simplify e1 .* simplify e2
simplify (Div e1 e2) = simplify e1 ./ simplify e2
simplify (Pow e1 e2) = simplify e1 .^ simplify e2



-- TODO also make another operator that is true for (3)(X) == (X)(3)
-- and 3 + x == x + 3

-- TODO make helper function to count number of "/" so we know whether to divide or multiply
-- the constants. So if odd "/" then divide else multiply.


-- TODO fix percolation so that we don't get order of operations wrong.

leftSub :: Tree a -> Tree a
leftSub (Node _ left _) = left
leftSub _ = error "leftSub"

rightSub :: Tree a -> Tree a
rightSub (Node _ _ right) = right
rightSub _ = error "rightSub"

isNode :: Tree a -> Bool
isNode (Node _ _ _) = True
isNode _ = False

isLeaf :: Tree a -> Bool
isLeaf (Leaf x) = True
isLeaf _ = False

leafVal :: Tree a -> a
leafVal (Leaf x) = x
leafVal _ = error "leafVal"

treeOp :: Tree a -> String
treeOp (Node op _ _) = op
treeOp _ = error "treeOp"

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

isThree :: Tree a -> Bool
isThree (Node n (Leaf x) (Leaf y)) = True
isThree _ = False


mapTree :: (Int -> Int) -> Tree Expr -> Tree Expr
mapTree _ Empty = Empty
mapTree f (Leaf (Num n)) = Leaf (Num (f n))
mapTree f leaf@(Leaf l) = leaf -- TODO extend tree so that if f = +1 ans sinx then 1 (Node +) sin x
mapTree f (Node n left right) = Node n (mapTree f left) (mapTree f right)



mkTree :: Expr -> Tree Expr
mkTree X = Leaf X
mkTree Y = Leaf Y
mkTree (Num n) = Leaf (Num n)
mkTree (F func) = Leaf (F func) -- TODO should I show expressions in functino as a tree as well?
mkTree (Neg (Num n)) = Leaf (Num (-n))
mkTree (Neg e) = Node "-" Empty (mkTree e)
mkTree (Add e1 e2) = Node "+" (mkTree e1) (mkTree e2)
mkTree (Sub e1 e2) = Node "-" (mkTree e1) (mkTree e2)
mkTree (Mul e1 e2) = Node "*" (mkTree e1) (mkTree e2)
mkTree (Div e1 e2) = Node "/" (mkTree e1) (mkTree e2)
mkTree (Pow e1 e2) = Node "^" (mkTree e1) (mkTree e2)


--- testing that mkTree and expr are inverses of each other.
getExpr :: Tree Expr -> Expr
getExpr (Leaf x) = x
getExpr (Node "-" Empty right) = Neg (getExpr right)
getExpr (Node "+" left right) = Add (getExpr left) (getExpr right)
getExpr (Node "-" left right) = Sub (getExpr left) (getExpr right)
getExpr (Node "*" left right) = Mul (getExpr left) (getExpr right)
getExpr (Node "/" left right) = Div (getExpr left) (getExpr right)
getExpr (Node "^" left right) = Pow (getExpr left) (getExpr right)



-- note: moves product/sum of constants to last level that a constant was present. Earlier constants
-- are replaced by Empty. Other node sor functions or variables are kept.
percolate :: Tree Expr -> Tree Expr
percolate (Node op (Leaf s) Empty) = Leaf s
percolate node@(Node op Empty (Leaf s)) = node
percolate (Node op Empty right) = Node op Empty (percolate right) -- might just be neg so we need struct
percolate (Node op left Empty) = percolate left
percolate (Node op left right)
    | op == "+" = perc (0 +) $ Node "+" left right
    | op == "-" = perc gSub $ Node "-" left right
    | op == "*" = perc (1 *) $ Node "*" left right
    | op == "/" = perc gDiv $ Node "/" left right
    | op == "^" = perc gPow $ Node "^" left right
    where gSub = \x -> x - 0
          gDiv = \x -> x `div` 1
          gPow = \x -> x ^ 1



-- assume no constants in the right subtree, just on the left, always, assume we have
-- already passed this tree into the rearrange const function
-- TODO need to fix this to avoid cases where op is lower precedence than function
-- example: op = + and we plus over leaf connected to a "*"
perc :: (Int -> Int) -> Tree Expr -> Tree Expr
perc f (Node op (Leaf (Num n)) (Leaf (Num m)))
    | op == "+" = Leaf (Num ((f n) + m))
    | op == "-" = Leaf (Num ((f n) - m))
    | op == "*" = Leaf (Num ((f n) * m))
    | op == "/" = Leaf (Num ((f n) `div` m))
    | op == "^" = Leaf (Num ((f n) ^ m))
perc f (Node op (Leaf (Num n)) leaf@(Leaf varOrFunc)) = Node op (Leaf (Num (f n))) leaf
perc f (Node op leaf@(Leaf varOrFunc) (Leaf (Num m))) = Node op leaf (Leaf (Num (f m)))
perc f (Node op v1@(Leaf varOrFunc1) v2@(Leaf varOrFunc2))
    | op == "+" = Leaf (Num (f 0)) --Node op (Node op Empty (Leaf (Num (f 0)))) (Node op v1 v2)
    | op == "-" = Leaf (Num (f 0))
    | op == "*" = Leaf (Num (f 1))
    | op == "/" = Leaf (Num (f 1))
    | op == "^" = Leaf (Num (f 1))
perc f (Node op (Leaf (Num n)) right) = Node op Empty (perc g right)
    where (g, _) = opFunc op f n
perc f (Node op left (Leaf (Num m))) = Node op (perc h left) Empty
    where (_, h) = opFunc op f m
perc f (Node op leaf@(Leaf varOrFunc) right) = Node op leaf (perc f right)
perc f (Node op left leaf@(Leaf varOrFunc)) = Node op (perc f left) leaf

perc f (Node op Empty right) = Node op Empty (perc f right)
perc f (Node op left Empty) = perc f left
-- note: the node-node cases
perc f (Node op left right) = Node op (perc f left) (perc f right)


opFunc :: String -> (Int -> Int) -> Int -> ((Int -> Int), (Int -> Int))
opFunc op f n
    | op == "+" = (((f n) +), (\x -> x + (f n)))
    | op == "-" = (((f n) -), (\x -> x - (f n)))
    | op == "*" = (((f n) *), (\x -> x * (f n)))
    | op == "/" = (((f n) `div`), (\x -> x `div` (f n)))
    | op == "^" = (((f n) ^), (\x -> x ^ (f n)))


-- makes tree shorter by removing given thing.
-- NOTE this is awesomely cool function ! Test thoroughly to make sure not missing any cases.
remove :: Tree Expr -> Tree Expr -> Tree Expr
remove t Empty = Empty
remove t (Leaf s) = if t == (Leaf s) then Empty else Leaf s
remove t node@(Node _ l1@(Leaf x) l2@(Leaf y))
    | l1 == t = l2
    | l2 == t = l1
    | otherwise = node
remove t node@(Node op l1@(Leaf x) right)
    | right == t = l1
    | otherwise = Node op l1 (remove t right)
remove t node@(Node op left l2@(Leaf y))
    | left == t = l2
    | otherwise = Node op (remove t left) l2
remove t node@(Node op left right)
    | t == left = remove t right
    | t == right = remove t left
    | otherwise = Node op (remove t left) (remove t right)


-- note findConst returns the first constant we find or else Nothing
-- precondition: has been passed to percolate first, so that the constant we get is the
-- last one in the tree.
findConst :: Tree Expr -> Maybe Int
findConst Empty = Nothing
findConst (Leaf (Num n)) = Just n
findConst (Leaf varOrFunc) = Nothing
findConst (Node op (Leaf (Num n)) (Leaf (Num m)))
    = error "findConst: not simplified! n, m on same branch"
findConst (Node op (Leaf (Num n)) leaf@(Leaf varOrFunc)) = Just n
findConst (Node op leaf@(Leaf varOrFunc) (Leaf (Num n)) ) = Just n
findConst (Node op (Leaf l1) (Leaf l2)) = Nothing
findConst (Node op (Leaf l) right) = findConst right
findConst (Node op left (Leaf l)) = findConst left
findConst (Node op left right) = error "help" -- TODO what to do?


-- note puts the constant given in the first available place (right under first node)
-- note if we have a leaf we must use the previous operator, not make one up!
crownTree :: Maybe String -> Int -> Tree Expr -> Tree Expr
crownTree opMaybe n tree
    | isNothing opMaybe = crown n tree
    | otherwise = Node (fromJust opMaybe) (Leaf (Num n)) tree
    where
    crown n Empty = Empty
    crown n (Leaf (Num m)) = Leaf (Num $ n + m) --- HELP todo
    crown n leaf@(Leaf varOrFunc) = Node "+" (Leaf (Num n)) leaf
    crown n tree@(Node op left right) = Node op (Leaf (Num n)) tree


-- precondition: passed to percolation so there are no patterns like Node num num.
-- precondition: passed to remove Empty _ after first thing so no Empty trees.
rearrangeConsts :: Tree Expr -> Tree Expr
rearrangeConsts Empty = Empty
rearrangeConsts (Leaf s) = Leaf s
rearrangeConsts tree
    | isLeaf tree' = tree'
    | isNothing constMaybe = Empty
    -- | isLeaf tree'' = crownTree opMaybe num tree''
    | otherwise = crownTree opMaybe num tree''
    where tree' = remove Empty $ percolate tree
          constMaybe = findConst tree'
          num = fromJust constMaybe
          (tree'', opMaybe) = if isThree tree'
                         then (remove (Leaf (Num num)) tree', Just $ treeOp tree')
                         else (remove (Leaf (Num num)) tree', Nothing)





---------------------------------------------------------------------------------------------------

instance Arbitrary Expr where
    arbitrary = sized arbExpr
arbExpr 0 = liftM Num arbitrary
arbExpr n = frequency [(2, return X), (2, return Y),
                       (1, liftM Num arbitrary),
                       (4, liftM Neg arbitrary),
                       (4, liftM2 Add (arbExpr (n `div` 2)) (arbExpr (n `div` 2))),
                       (4, liftM2 Sub (arbExpr (n `div` 2)) (arbExpr (n `div` 2))),
                       (4, liftM2 Mul (arbExpr (n `div` 2)) (arbExpr (n `div` 2))),
                       (4, liftM2 Div (arbExpr (n `div` 2)) (arbExpr (n `div` 2))),
                       (4, liftM2 Pow (arbExpr (n `div` 2)) (arbExpr (n `div` 2)))]


instance Arbitrary Function where
    arbitrary = sized arbFunc
arbFunc n = frequency [(4, liftM Sin arbitrary),
                       (4, liftM Cos arbitrary),
                       (4, liftM Tan arbitrary),
                       (4, liftM Csc arbitrary),
                       (4, liftM Sec arbitrary),
                       (4, liftM Cot arbitrary),
                       (4, liftM Arcsin arbitrary),
                       (4, liftM Arccos arbitrary),
                       (4, liftM Arctan arbitrary),
                       (4, liftM Arccsc arbitrary),
                       (4, liftM Arcsec arbitrary),
                       (4, liftM Arctan arbitrary)]


instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbTree
arbTree 0 = return Empty --liftM NilT arbitrary
arbTree n = frequency [(1, return Empty),
                       (3, liftM Leaf arbitrary),
                       (4, liftM3 Node arbitrary (arbTree (n `div` 2))
                                                 (arbTree (n `div` 2)) )]
