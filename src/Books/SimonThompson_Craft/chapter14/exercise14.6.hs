
data Expr = Lit Integer
          | Op Ops Expr Expr deriving (Eq, Show)

data Ops = Add | Sub | Mul | Div | Mod deriving (Eq, Show)



expr1, expr2, expr3, expr4, expr5, expr6, expr7, expr8, expr9 :: Expr
expr1 = Lit 2                             -- 2
expr2 = Op Add (Lit 2) (Lit 3)               -- 2 + 3
expr3 = Op Add (Op Sub (Lit 3) (Lit 1)) (Lit 3) -- (3-1) + 3
expr4 = Op Add (Op Add (Lit 2) (Lit 3)) (Lit 4)
expr5 = Op Add expr4 (Lit 5)
expr6 = Op Add (Op Add (Op Sub (Lit 2) (Op Add (Op Add (Lit 6) (Lit 7))
                (Lit 8))) (Lit 4)) (Lit 5)

expr7 = Op Add (Op Sub (Lit 1) (Op Div (Op Mul (Lit 7) (Lit 8)) (Lit 2) )) (Lit 3) -- (-24)
expr8 = Op Mul (Op Div (Lit 14) (Lit 2)) (Op Add (Lit 3) (Lit 10)) -- 91
expr9 = Op Div (Lit 30) (Op Div (Lit 125) (Lit 25)) -- 6

expr10 = Op Div (Op Mod (Lit 125) (Lit 35)) (Lit 4)

-- note counts number of operators in an expression
size :: Expr -> Integer
size (Lit n) = 0
size (Op _ e1 e2) = 1 + size e1 + size e2


eval :: Expr -> Integer
eval (Lit n) = n
eval (Op Add e1 e2) = (eval e1) + (eval e2)
eval (Op Sub e1 e2) = (eval e1) - (eval e2)
eval (Op Mul e1 e2) = (eval e1) * (eval e2)
eval (Op Mod e1 e2) = (eval e1) `mod` (eval e2)
eval (Op Div e1 e2)
    | sndExprNum /= 0 = (eval e1) `div` sndExprNum
    | otherwise = error ("Divide by zero: " ++ (showExpr e2))
    where sndExprNum = (eval e2)



showExpr :: Expr -> String
showExpr expr = sho expr 0

sho :: Expr -> Int -> String
sho (Lit n) c = show n
sho (Op Add e1 e2) c = applySho c (sho e1 (c+1) ++ " + " ++ sho e2 (c+1))
sho (Op Sub e1 e2) c = applySho c (sho e1 (c+1) ++ " - " ++ sho e2 (c+1))
sho (Op Mul e1 e2) c = applySho c (sho e1 (c+1) ++ " * " ++ sho e2 (c+1))
sho (Op Div e1 e2) c = applySho c (sho e1 (c+1) ++ " / " ++ sho e2 (c+1))
sho (Op Mod e1 e2) c = applySho c (sho e1 (c+1) ++ " % " ++ sho e2 (c+1))

applySho :: Int -> String -> String
applySho count exprStr
    | count == 0 = exprStr
    | otherwise = applyOuterParens exprStr

applyOuterParens :: String -> String
applyOuterParens s = "(" ++ s ++ ")"


