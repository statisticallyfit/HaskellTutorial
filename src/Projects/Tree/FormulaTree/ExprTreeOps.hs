module ExprTreeOps where

-- data Variable = Ω | X -- | ω | Φ | χ | γ | α | β | θ


data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr |
    Div Expr Expr | Pow Expr Expr | Neg Expr | Num Int  {-Var Expr-} | X | Y
    deriving (Eq, Show)


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

{-
NOTE:
be able to fold over tree and simplify constants but also remember order that they
were in the tree!
Then remember also where they go at the end!
-}

-- (f . (n * ))
-- example: foldLeftTree $ getConsts() $ 4xsin(x)3cos(x) => 12
foldLeftTree :: {-(Int -> Int -> Int) ->-} Int -> Tree Expr -> Int
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



-- data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr
instance Arbitrary Expr where
    arbitrary = sized arbExpr
arbExpr 0 = liftM Lit arbitrary
arbExpr n = frequency [(1, liftM Lit arbitrary),
                       (4, liftM2 Add (arbExpr (n `div` 2)) (arbExpr (n `div` 2))),
                       (4, liftM2 Sub (arbExpr (n `div` 2)) (arbExpr (n `div` 2)))]