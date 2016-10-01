module ExprTreeOps where

import Test.QuickCheck -- (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad

-- data Variable = Ω | X -- | ω | Φ | χ | γ | α | β | θ

data Function = Sin Expr | Cos Expr | Tan Expr
    deriving (Eq, Show)

data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
    | Pow Expr Expr | Neg Expr | Num Int  {-Var Expr-} | X | Y | F Function
    deriving (Eq, Show)

(.+), (.-), (.*), (./), (.^) :: Expr -> Expr -> Expr
(.+) = Add
(.-) = Sub
(.*) = Mul
(./) = Div
(.^) = Pow


data Tree a = Empty | Leaf a | Node String (Tree a) (Tree a) deriving (Eq, Show)


t1, t2 :: Tree Expr
-- 4x + 3x^5
t1 = Node "+"
        (Node "*" (Leaf (Num 4)) (Leaf X))
        (Node "*" (Leaf (Num 3)) (Node "^" (Leaf X) (Leaf (Num 5))))

t2 = Node "*"
        (Leaf (Num 4))
        (Node "*"
            (Leaf X)
            (Node "*"
                (Leaf (Num 3))
                (Leaf Y)))

-- TODO how to represent the associativity of types?


e1 :: Expr
e1 = Num(4) .+ X .* (F (Sin X)) .* Num(3) .* (F (Cos X))
e2 = e1 .+ Num(2) .* X .+ Num(7) .* X

{-
NOTE:
be able to fold over tree and simplify constants but also remember order that they
were in the tree!
Then remember also where they go at the end!
-}




mkTree :: Expr -> Tree Expr
mkTree X = Leaf X
mkTree Y = Leaf Y
mkTree (Num n) = Leaf (Num n)
mkTree (Neg e) = Node "_" Empty (mkTree e)
mkTree (Add e1 e2) = Node "+" (mkTree e1) (mkTree e2)
mkTree (Sub e1 e2) = Node "-" (mkTree e1) (mkTree e2)
mkTree (Mul e1 e2) = Node "*" (mkTree e1) (mkTree e2)
mkTree (Div e1 e2) = Node "/" (mkTree e1) (mkTree e2)





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

{-

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
                       (4, liftM2 Pow (arbExpr (n `div` 2)) (arbExpr (n `div` 2)))]-}
