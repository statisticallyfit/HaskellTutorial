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

-- TODO idea:
-- percolate constant multiplication result to the lowest
-- last constant's position and return tree with the result
-- constant in the last constant's position.
-- THen, do separate function that moves the result back up to the
-- where the first constant was located (now marked with Empty)
-- So we are shifting the tree upwards.

-- if we found a constant, percolate the rest, if they exist...
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
-- note: the num-num cases
perc f (Node op (Leaf (Num n)) (Leaf (Num m)))
    | op == "+" = Leaf (Num ((f n) + m))
    | op == "-" = Leaf (Num ((f n) - m))
    | op == "*" = Leaf (Num ((f n) * m))
    | op == "/" = Leaf (Num ((f n) `div` m))
    | op == "^" = Leaf (Num ((f n) ^ m))
perc f (Node op (Leaf (Num n)) leaf@(Leaf varOrFunc))
    | op == "+" = Node "+" (Leaf (Num (f n))) leaf
    | op == "-" = Node "-" (Leaf (Num (f n))) leaf
    | op == "*" = Node "*" (Leaf (Num (f n))) leaf
    | op == "/" = Node "/" (Leaf (Num (f n))) leaf
    | op == "^" = Node "^" (Leaf (Num (f n))) leaf
perc f (Node op leaf@(Leaf varOrFunc) (Leaf (Num m)))
    | op == "+" = Node "+" leaf (Leaf (Num (f m)))
    | op == "-" = Node "-" leaf (Leaf (Num (f m)))
    | op == "*" = Node "*" leaf (Leaf (Num (f m)))
    | op == "/" = Node "/" leaf (Leaf (Num (f m)))
    | op == "^" = Node "^" leaf (Leaf (Num (f m)))
perc f (Node op v1@(Leaf varOrFunc1) v2@(Leaf varOrFunc2))
    | op == "+" = Leaf (Num (f 0)) --Node op (Node op Empty (Leaf (Num (f 0)))) (Node op v1 v2)
    | op == "-" = Leaf (Num (f 0))
    | op == "*" = Leaf (Num (f 1))
    | op == "/" = Leaf (Num (f 1))
    | op == "^" = Leaf (Num (f 1))
perc f (Node op (Leaf (Num n)) right)
    | op == "+" = Node "+" Empty (perc ((f n) +) right)
    | op == "-" = Node "-" Empty (perc ((f n) -) right)
    | op == "*" = Node "*" Empty (perc ((f n) *) right)
    | op == "/" = Node "/" Empty (perc ((f n) `div`) right)
    | op == "^" = Node "^" Empty (perc ((f n) ^) right)
perc f (Node op left (Leaf (Num m)))
    | op == "+" = Node "+" (perc (\x -> x + (f m)) left) Empty
    | op == "-" = Node "-" (perc (\x -> x - (f m)) left) Empty
    | op == "*" = Node "*" (perc (\x -> x * (f m)) left) Empty
    | op == "/" = Node "/" (perc (\x -> x `div` (f m)) left) Empty
    | op == "^" = Node "^" (perc (\x -> x ^ (f m)) left) Empty
perc f (Node op leaf@(Leaf varOrFunc) right)
    | op == "+" = Node "+" leaf (perc f right)
    | op == "-" = Node "-" leaf (perc f right)
    | op == "*" = Node "*" leaf (perc f right)
    | op == "/" = Node "/" leaf (perc f right)
    | op == "^" = Node "^" leaf (perc f right)
perc f (Node op left leaf@(Leaf varOrFunc))
    | op == "+" = Node "+" (perc f left) leaf
    | op == "-" = Node "-" (perc f left) leaf
    | op == "*" = Node "*" (perc f left) leaf
    | op == "/" = Node "/" (perc f left) leaf
    | op == "^" = Node "^" (perc f left) leaf

perc f (Node op Empty right) = Node op Empty (perc f right)
perc f (Node op left Empty) = perc f left
-- note: the node-node cases
perc f (Node op left right) = Node op (perc f left) (perc f right)
{-perc f (Node op (Leaf (Num n)) leaf@(Leaf varOrFunc)) = Node op (Leaf (Num (f n))) leaf
perc f (Node op leaf@(Leaf varOrFunc) (Leaf (Num m))) = Node op leaf (Leaf (Num (f m)))
perc f (Node op (Leaf (Num n)) right) = Node op Empty (perc g right)
    where (g, _) = opFunc op f n
perc f (Node op left (Leaf (Num m))) = Node op (perc h left) Empty
    where (_, h) = opFunc op f m
perc f (Node op leaf@(Leaf varOrFunc) right) = Node op leaf (perc f right)
perc f (Node op left leaf@(Leaf varOrFunc)) = Node op (perc f left) leaf-}


-- perc f (Node _ _ _) = Leaf (Num (f 1))


opFunc :: String -> (Int -> Int) -> Int -> ((Int -> Int), (Int -> Int))
opFunc op f n
    | op == "+" = (((f n) +), (\x -> x + (f n)))
    | op == "-" = (((f n) -), (\x -> x - (f n)))
    | op == "*" = (((f n) *), (\x -> x * (f n)))
    | op == "/" = (((f n) `div`), (\x -> x `div` (f n)))
    | op == "^" = (((f n) ^), (\x -> x ^ (f n)))


{- NOTe all together (same times together)
percolate (Node "+" (Leaf (Num n)) (Leaf (Num m))) = Leaf $ Num $ n + m
percolate (Node "+" (Leaf (Num n)) right) = Node "+" Empty (perc (n +) right)
percolate (Node "+" left (Leaf (Num n))) = Node "+" (perc (n +) left) Empty
percolate (Node "+" leaf@(Leaf varOrFunc) right) = Node "+" leaf (perc id right)
percolate (Node "+" left right) = Node "+" (percolate left) (percolate right)

percolate (Node "-" (Leaf (Num n)) (Leaf (Num m))) = Leaf $ Num $ n - m
percolate (Node "-" (Leaf (Num n)) right) = Node "-" Empty (perc (n -) right)
percolate (Node "-" left (Leaf (Num n))) = Node "-" Empty (perc (\x -> x - n) left)
percolate (Node "-" leaf@(Leaf varOrFunc) right) = Node "+" leaf (perc id right)
percolate (Node "-" left right) = Node "-" (percolate left) (percolate right)

percolate (Node "*" (Leaf (Num n)) (Leaf (Num m))) = Leaf $ Num $ n * m
percolate (Node "*" (Leaf (Num n)) right) = Node "*" Empty (perc (n *) right)
percolate (Node "*" left (Leaf (Num n))) = Node "*" Empty (perc (n *) left)
percolate (Node "*" leaf@(Leaf varOrFunc) right) = Node "*" leaf (perc id right)
percolate (Node "*" left right) = Node "*" (percolate left) (percolate right)

percolate (Node "/" (Leaf (Num n)) (Leaf (Num m))) = Leaf $ Num $ n `div` m
percolate (Node "/" (Leaf (Num n)) right) = Node "/" Empty (perc (n `div`) right)
percolate (Node "/" left (Leaf (Num n))) = Node "/" Empty (perc (\x -> x `div` n) left)
percolate (Node "/" leaf@(Leaf varOrFunc) right) = Node "/" leaf (perc id right)
percolate (Node "/" left right) = Node "/" (percolate left) (percolate right)

percolate (Node "^" (Leaf (Num n)) (Leaf (Num m))) = Leaf $ Num $ n ^ m
percolate (Node "^" (Leaf (Num n)) right) = Node "^" Empty (perc (n ^) right)
percolate (Node "^" left (Leaf (Num n))) = Node "^" Empty (perc (\x -> x ^ n) left)
percolate (Node "^" leaf@(Leaf varOrFunc) right) = Node "^" leaf (perc id right)
percolate (Node "^" left right) = Node "^" (percolate left) (percolate right)
-}

{-
-- note: the meat add cases
perc f (Node "+" (Leaf (Num n)) leaf@(Leaf varOrFunc)) = Node "+" (Leaf (Num (f n))) leaf
perc f (Node "+" leaf@(Leaf varOrFunc) (Leaf (Num m))) = Node "+" leaf (Leaf (Num (f m)))
perc f (Node "+" (Leaf (Num n)) right) = Node "+" Empty (perc ((f n) +) right)
perc f (Node "+" left (Leaf (Num m))) = Node "+" (perc (\x -> x + (f m)) left) Empty
perc f (Node "+" leaf@(Leaf varOrFunc) right) = Node "+" leaf (perc f right)
perc f (Node "+" left leaf@(Leaf varOrFunc)) = Node "+" (perc f left) leaf
-- note: the meat sub cases
perc f (Node "-" (Leaf (Num n)) leaf@(Leaf varOrFunc)) = Node "-" (Leaf (Num (f n))) leaf
perc f (Node "-" leaf@(Leaf varOrFunc) (Leaf (Num m))) = Node "-" leaf (Leaf (Num (f m)))
perc f (Node "-" (Leaf (Num n)) right) = Node "-" Empty (perc ((f n) -) right)
perc f (Node "-" left (Leaf (Num m))) = Node "-" (perc (\x -> x - (f m)) left) Empty
perc f (Node "-" leaf@(Leaf varOrFunc) right) = Node "-" leaf (perc f right)
perc f (Node "-" left leaf@(Leaf varOrFunc)) = Node "-" (perc f left) leaf
-- note: the meat mult cases
perc f (Node "*" (Leaf (Num n)) leaf@(Leaf varOrFunc)) = Node "*" (Leaf (Num (f n))) leaf
perc f (Node "*" leaf@(Leaf varOrFunc) (Leaf (Num m))) = Node "*" leaf (Leaf (Num (f m)))
perc f (Node "*" (Leaf (Num n)) right) = Node "*" Empty (perc ((f n) *) right)
perc f (Node "*" left (Leaf (Num m))) = Node "*" (perc ((f m) *) left) Empty
perc f (Node "*" leaf@(Leaf varOrFunc) right) = Node "*" leaf (perc f right)
perc f (Node "*" left leaf@(Leaf varOrFunc)) = Node "*" (perc f left) leaf
-- note: the meat div cases
perc f (Node "/" (Leaf (Num n)) leaf@(Leaf varOrFunc)) = Node "/" (Leaf (Num (f n))) leaf
perc f (Node "/" leaf@(Leaf varOrFunc) (Leaf (Num m))) = Node "/" leaf (Leaf (Num (f m)))
perc f (Node "/" (Leaf (Num n)) right) = Node "/" Empty (perc ((f n) `div`) right)
perc f (Node "/" left (Leaf (Num m))) = Node "/" (perc (\x -> x `div` (f m)) left) Empty
perc f (Node "/" leaf@(Leaf varOrFunc) right) = Node "/" leaf (perc f right)
perc f (Node "/" left leaf@(Leaf varOrFunc)) = Node "/" (perc f left) leaf
-- note: the meat pow cases
perc f (Node "^" (Leaf (Num n)) leaf@(Leaf varOrFunc)) = Node "^" (Leaf (Num (f n))) leaf
perc f (Node "^" leaf@(Leaf varOrFunc) (Leaf (Num m))) = Node "^" leaf (Leaf (Num (f m)))
perc f (Node "^" (Leaf (Num n)) right) = Node "^" Empty (perc ((f n) ^) right)
perc f (Node "^" left (Leaf (Num m))) = Node "^" (perc (\x -> x ^ (f m)) left) Empty
perc f (Node "^" leaf@(Leaf varOrFunc) right) = Node "^" leaf (perc f right)
perc f (Node "^" left leaf@(Leaf varOrFunc)) = Node "^" (perc f left) leaf
-}





-- TODO idea: make array holding coefficients of powers
-- so that Array (2, 0, 1, 5) means 5x^3 + x^2 + 2
-- then we can add/sub/mul/div/pow.
-- Once we simplify the expression this way (single elements only)
-- then we can use the tree to simplify things like x^2 * sin^3(2x)
-- by providing a ~= operator that is true if the structure is the same.

-- TODO also make another operator that is true for (3)(X) == (X)(3)
-- and 3 + x == x + 3

-- TODO make helper function to count number of "/" so we know whether to divide or multiply
-- the constants. So if odd "/" then divide else multiply.


-- makes tree shorter by removing given thing.
{-
squash :: Tree Expr -> Tree Expr
squash node@(Node op (Leaf l1) (Leaf l2)) = node
squash (Node op Empty leaf@(Leaf l)) = leaf
squash (Node op Empty right) = squash right
squash (Node op leaf@(Leaf l) right) = Node op leaf (squash right)
-}

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


-- findConst returns the first constant we find and puts Empty in its place.
-- (after percolate, it is at bottom)
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


-- puts the constant given in the first available place (right under first node)
crownTree :: Maybe String -> Int -> Tree Expr -> Tree Expr
crownTree opMaybe n tree
    | isNothing opMaybe = crown' n tree
    | otherwise = Node (fromJust opMaybe) (Leaf (Num n)) tree
    where
    crown' n Empty = Empty
    crown' n (Leaf (Num m)) = Leaf (Num $ n + m) --- HELP todo
    crown' n leaf@(Leaf varOrFunc) = Node "+" (Leaf (Num n)) leaf
    crown' n tree@(Node op left right) = Node op (Leaf (Num n)) tree


-- assume should be no unsimplified constants like Node num num left (passed to perc first)
-- assume no Empty , so has been passed to squash first.
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






{-

-- note yields in order results
foldrIn :: (a -> b -> b) -> b -> Tree a -> b
foldrIn _ acc Nil = acc
foldrIn f acc (Node n left right) = foldrIn f (f n (foldrIn f acc right)) left

-- note if you want forward inorder, not backwards, put right as inner and left outer.
foldlIn :: (b -> a -> b) -> b -> Tree a -> b
foldlIn _ acc Nil = acc
foldlIn f acc (Node n left right) = foldlIn f (f (foldlIn f acc right) n) left

foldrPost :: (a -> b -> b) -> b -> Tree a -> b
foldrPost _ acc Nil = acc
foldrPost f acc (Node n left right) = foldrPost f (foldrPost f (f n acc) right) left

foldlPost :: (b -> a -> b) -> b -> Tree a -> b
foldlPost _ acc Nil = acc
foldlPost f acc (Node n left right) = foldlPost f (foldlPost f (f acc n) right) left

-- todo fix so they print in order
foldrPre :: (a -> b -> b) -> b -> Tree a -> b
foldrPre _ acc Nil = acc
foldrPre f acc (Node n left right) = foldrPre f (foldrPre f (f n acc) left) right

-- todo fix so it prints in order
-- foldl f (foldl f (f z n) left) right
foldlPre :: (b -> a -> b) -> b -> Tree a -> b
foldlPre _ acc Nil = acc
foldlPre f acc (Node n left right) = foldlPre f (foldlPre f (f acc n) left) right
-}


{-

traverseTree :: (a -> (b -> b) -> (b -> b) -> b -> b) -> (t -> a) -> b -> Tree t -> b
traverseTree step f z tree = go tree z
    where
    go Empty z = z
    go (Leaf x) z = step (f x) (go Empty) (go Empty) z
    go (Node x l r) z = step (f x) (go l) (go r) z


preorder, inorder, postorder :: (a -> b -> b) -> b -> Tree a -> b
preorder   = traverseTree $ \n l r -> n . l . r  -- r . l . n
inorder    = traverseTree $ \n l r -> l . n . r  -- r . n . l
postorder  = traverseTree $ \n l r -> l . r . n  -- n . r . l
-}




-- (f . (n * ))
-- example: foldLeftTree $ getConsts() $ 4xsin(x)3cos(x) => 12
--foldLeftTree :: {-(Int -> Int -> Int) ->-} Int -> Tree Expr -> Int
--foldLeftTree s (Leaf(Num n)) = s n
{-foldLeftTree s (Node "*" (Leaf(Num n)) right) = foldLeftTree (s * n) right
foldLeftTree s (Node "/" (Leaf(Num n)) right) = foldLeftTree (s `div` n) right
foldLeftTree s (Node _ (Leaf X) right) = foldLeftTree s right
foldLeftTree s (Leaf (Num n)) = -}
--foldLeftTree f (Node op left right) = foldLeftTree (f . )



{-
main :: IO()
main = do
    print $ foldLeftTree (\x -> )-}


{-

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)


instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node left a right) = (foldMap f left) <> (f a) <> (foldMap f right)

    foldl _ z Empty = z
    foldl f z (Leaf a) = f z a
    foldl f z (Node left a right) = foldl f (foldl f (f z a) left) right

    foldr _ z Empty = z
    foldr f z (Leaf a) = f a z
    foldr f z (Node left a right) = foldr f (f a (foldr f z right)) left
-}



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
