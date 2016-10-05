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
      Sinh Expr | Cosh Expr | Tanh Expr |
      Csch Expr | Sech Expr | Coth Expr |
      Arcsinh Expr | Arccosh Expr | Arctanh Expr |
      Arccsch Expr | Arcsech Expr | Arccoth Expr |
      Ln Expr | E Expr | Log Expr Expr -- first expr is base
    deriving (Eq)


data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
    | Pow Expr Expr | Neg Expr | Num Int  {-Var Expr-} | X | Y | F Function
    deriving (Eq)

type Coeff = Int
type Exps = Int

data ExprHolder = Polynomial [Coeff] | Trig [(Coeff, Exps, Expr)] | InvTrig [(Coeff, Exps, Expr)] |
    Hyperbolic [(Coeff, Exps, Expr)] | InvHyp [(Coeff, Exps, Expr)] | Logarithmic [(Coeff, Exps, Expr)]
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
    show (Mul (Num n) (Num m)) = "(" ++ show n ++ ")(" ++ show m ++ ")"
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
    show (Sinh e) = "sinh(" ++ show e ++ ")"
    show (Cosh e) = "cosh(" ++ show e ++ ")"
    show (Tanh e) = "tanh(" ++ show e ++ ")"
    show (Csch e) = "csch(" ++ show e ++ ")"
    show (Sech e) = "sech(" ++ show e ++ ")"
    show (Coth e) = "coth(" ++ show e ++ ")"
    show (Arcsinh e) = "arcsinh(" ++ show e ++ ")"
    show (Arccosh e) = "arccosh(" ++ show e ++ ")"
    show (Arctanh e) = "arctanh(" ++ show e ++ ")"
    show (Arccsch e) = "arccsch(" ++ show e ++ ")"
    show (Arcsech e) = "arcsech(" ++ show e ++ ")"
    show (Arccoth e) = "arccoth(" ++ show e ++ ")"
    show (E e) = "e^(" ++ show e ++ ")"
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

exprHolder :: Expr -> ExprHolder
exprHolder
exprHolder (Add e1 e2) =
exprHolder (Sub e1 e2) =
-}

-- note in cases like 4x + sin(3x) is just separates polynomials from trig and log so that we just
-- simplify those.
-- takes something like single expression 2sin(3x) + 4sin(3x) = 6sin(3x)
-- TODO: fix holderToExpr to handle 2xsin(3x) things then update this one too.
-- TODO help when given multiplication or division or powers. won't know how to parse
-- into expression holder.
-- IMPORTANT oh wait, i made multiplication left associative  so
-- will the expression look like this (2) (xsin(3x)) so that it will work for this function?

-- TODO correct to take exprholder (holds poly, trig..)
-- TODO now to convert to expression, unzip, get rid of justa nd conver nothings to 0.

addH :: [(Coeff, Exps, Expr)] -> [(Coeff, Exps, Expr)] -> Expr
addH h1 h2 = map simpMaybe $ map add (zip h1' h2')
    where
    h1' = map numify h1
    h2' = map numify h2
    add (a@(c1,e1,x1),b@(c2,e2,x2)) =
        if x1 == x2 then (Just (c1 .+ c2,e1,x1), Nothing)
        else (Just a, Just b)
    simpFirst (x,y,z) = (simplify x, y, z)
    simpMaybe (a,b) =
        if (isJust a && isNothing b) then (Just $ simpFirst $ fromJust a, Nothing)
        else (a,b)

list = map numify [(4,1,X), (3,1,X), (1,7,(Num 2) .* X .^ Num 5), (2,2,X),(1,1,X), (7,7,Num 7 .* X)]
add (a@(c1,e1,x1),b@(c2,e2,x2)) = if x1 == x2 then (Just (c1 .+ c2,e1,x1), Nothing) else (Just a, Just b)
hs = zip list list
js = map add hs
simp' (x,y,z) = (simplify x, y, z)
ms = map (\(a, b) -> if (isJust a && isNothing b) then (Just $ simp' $ fromJust a, Nothing) else (a,b)) js
--g (a@(c1,e1,x1), b@(c2,e2,x2)) = if (x1 == x2) then (c1 .+ c2,e1,x1) else (a,b)

-- TODO make polynomials and trigs and other functions be raised to powers of functions
-- not just constants (make the middle element in tuple to be an expression)
-- While you're at it, make the first coeff an expression so that we can do xsin(x) + 2xsin(x)
-- TODO return error if one of the tuples has patterns (1, 0, ..) because that yields a constant.
-- NOTE rules:
-- POlynomial: [1, 0, 2, 7, -4] represents 1 + x^2 + 7x^3 - 4x^4
-- Trig: [(0,1), (1,1), (4,1), (1,1), (2,4), (3, 5)]
--      represents (cos x + 4tan x + csc x + 2sec^4 x  + 3cot^5 x
--      where x = any expression and note (0, 1) = 0 always and (1,0) gets error
holderToExpr :: ExprHolder -> [Expr]
holderToExpr (Polynomial ps) = reverse $ map simplify $ zipWith (.*) ps' (zipWith (.^) xs es )
    where xs = replicate (length ps) X
          es = map Num [0 .. (length ps - 1)]
          ps' = map Num ps
-- TODO inside function is holder -- find way to deal with that.
holderToExpr (Trig ts) = map simplify $ latch (zip ts fs)
    where fs = map F [Sin X, Cos X, Tan X, Csc X, Sec X, Cot X]
holderToExpr (InvTrig ts) = map simplify $ latch (zip ts fs)
    where fs = map F [Arcsin X, Arccos X, Arctan X, Arccsc X, Arcsec X, Arccot X]
holderToExpr (Hyperbolic hs) = map simplify $ latch (zip hs fs)
    where fs = map F [Sinh X, Cosh X, Tanh X, Csch X, Sech X, Coth X]
holderToExpr (InvHyp hs) = map simplify $ latch  (zip hs fs)
    where fs = map F [Arcsinh X, Arccosh X, Arctanh X, Arccsch X, Arcsech X, Arccoth X]
holderToExpr (Logarithmic ls) = map simplify $ latch (zip ls fs)
    where fs = map F [E X, Ln X, Log X X]  -- TODO fix so we can have different args for log base and arg.


latch zs = map (\((c,e,u), f) -> c .* (push u f) .^ e) (map (\((tup, f)) -> (numify tup, f)) zs)
numify (c,e,u) = (Num c, Num e, u)


-- TODO types wrong
--- insdie the function is a type expression holder that is converted to expression.
{-holderExprFunctions :: Expr -> Expr
holderExprFunctions (F (Sin h)) = -}


-- puts an expression inside a function
push :: Expr -> Expr -> Expr
push u (F (Sin v)) = F $ Sin u
push u (F (Cos v)) = F $ Cos u
push u (F (Tan v)) = F $ Tan u
push u (F (Csc v)) = F $ Csc u
push u (F (Sec v)) = F $ Sec u
push u (F (Cot v)) = F $ Cot u
push u (F (Arcsin v)) = F $ Arcsin u
push u (F (Arccos v)) = F $ Arccos u
push u (F (Arctan v)) = F $ Arctan u
push u (F (Arccsc v)) = F $ Arccsc u
push u (F (Arcsec v)) = F $ Arcsec u
push u (F (Arccot v)) = F $ Arccot u
push u (F (Sinh v)) = F $ Sinh u
push u (F (Cosh v)) = F $ Cosh u
push u (F (Tanh v)) = F $ Tanh u
push u (F (Csch v)) = F $ Csch u
push u (F (Sech v)) = F $ Sech u
push u (F (Coth v)) = F $ Coth u
push u (F (Arcsinh v)) = F $ Arcsinh u
push u (F (Arccosh v)) = F $ Arccosh u
push u (F (Arctanh v)) = F $ Arctanh u
push u (F (Arccsch v)) = F $ Arccsch u
push u (F (Arcsech v)) = F $ Arcsech u
push u (F (Arccoth v)) = F $ Arccoth u
push u (F (E v)) = F $ E u
push u (F (Ln v)) = F $ Ln u
push u (F (Log v1 v2)) = F $ Log u u


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
simplify (F (Sinh e)) = F $ Sinh $ simplify e
simplify (F (Cosh e)) = F $ Cosh $ simplify e
simplify (F (Tanh e)) = F $ Tanh $ simplify e
simplify (F (Csch e)) = F $ Csch $ simplify e
simplify (F (Sech e)) = F $ Sech $ simplify e
simplify (F (Coth e)) = F $ Coth $ simplify e
simplify (F (Arcsinh e)) = F $ Arcsinh $ simplify e
simplify (F (Arccosh e)) = F $ Arccosh $ simplify e
simplify (F (Arctanh e)) = F $ Arctanh $ simplify e
simplify (F (Arccsch e)) = F $ Arccsch $ simplify e
simplify (F (Arcsech e)) = F $ Arcsech $ simplify e
simplify (F (Arccoth e)) = F $ Arccoth $ simplify e
simplify (F (E e)) = F $ E $ simplify e
simplify (F (Ln e)) = F $ Ln $ simplify e
simplify (F (Log e1 e2)) = F $ Log (simplify e1) (simplify e2)

-- TODO may not need these cases once I do operations on expression holders of arrays.
simplify (Add (Num n) (Num m)) = Num $ n + m
simplify (Add (Mul (Num a) (Num b)) (Mul (Num c) (Num d))) = Num $ a * b + c + d
simplify (Add (Mul (Num a) x) (Mul (Num b) y))
    = if x == y then (Num (a + b) .* simplify x) else ((Num a) .* simplify x .+ (Num b) .* simplify y)
simplify (Add (Mul (Num a) x) (Mul y (Num b))) = simplify ((Num a) .* x .+ (Num b) .* y)
simplify (Add (Mul x (Num a)) (Mul (Num b) y)) = simplify ((Num a) .* x .+ (Num b) .* y)
simplify (Add (Mul x (Num a)) (Mul y (Num b))) = simplify ((Num a) .* x .+ (Num b) .* y)
simplify (Add x y) = if x == y then (Num 2 .* simplify x) else (simplify x .+ simplify y)

simplify (Sub (Num n) (Num m)) = Num $ n - m
simplify (Sub (Mul (Num a) (Num b)) (Mul (Num c) (Num d))) = Num $ a * b - c + d
simplify (Sub (Mul (Num a) x) (Mul (Num b) y))
    = if x == y then (Num (a - b) .* simplify x) else ((Num a) .* simplify x .- (Num b) .* simplify y)
simplify (Sub (Mul (Num a) x) (Mul y (Num b))) = simplify ((Num a) .* x .- (Num b) .* y)
simplify (Sub (Mul x (Num a)) (Mul (Num b) y)) = simplify ((Num a) .* x .- (Num b) .* y)
simplify (Sub (Mul x (Num a)) (Mul y (Num b))) = simplify ((Num a) .* x .- (Num b) .* y)
simplify (Sub x y) = if x == y then (Num 0) else (simplify x .- simplify y)

simplify (Mul (Num 1) e2) = simplify e2
simplify (Mul e1 (Num 1)) = simplify e1
simplify (Mul (Num 0) e2) = Num 0
simplify (Mul e1 (Num 0)) = Num 0
simplify (Mul (Num n) (Num m)) = Num $ n * m
simplify (Mul (Neg u) (Neg v)) = simplify u .* simplify v
simplify (Mul (Neg u) v) = Neg (simplify u .* simplify v)
simplify (Mul u (Neg v)) = Neg (simplify u .* simplify v)
simplify (Mul x y) = if x == y then simplify x .* (Num 2) else simplify x .* simplify y

simplify (Div (Num 0) (Num 0)) = error "0/0 not possible"
simplify (Div (Num n) (Num m)) = Num $ n `div` m
simplify (Div (Num 0) e2) = Num 0
simplify (Div e1 (Num 0)) = error "divide by zero!"
simplify (Div e1 (Num 1)) = simplify e1
simplify (Div x y) = if x == y then (Num 1) else simplify x ./ simplify y

simplify (Pow (Num n) (Num m)) = Num $ n ^ m
simplify (Pow e (Num 0)) = Num 1
simplify (Pow (Num 0) e) = Num 0
simplify (Pow (Num 1) e) = Num 1
simplify (Pow e (Num 1)) = simplify e --- TODO do power rules next
simplify (Pow x y) = simplify x .^ simplify y

simplify (Neg (Num n)) = Num (-n)
simplify (Neg (Neg e)) = simplify e
simplify (Neg e) = Neg $ simplify e



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
