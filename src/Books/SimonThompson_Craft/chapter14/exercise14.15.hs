
-- If b e1 e2 has value e1 if b == True and has value e2 if b == False.
data Expr = Lit Integer
          | Op Ops Expr Expr
          | If BoolExpr Expr Expr
          deriving (Eq, Show)

data Ops = Add | Sub | Mul | Div | Mod deriving (Eq, Show)

data BoolExpr = BoolLit Bool
              | And BoolExpr BoolExpr -- true when both bool exprs are true
              | Not BoolExpr          -- true when boolexpr is false
              | Equal Expr Expr       -- true when both exprs are equal
              | Greater Expr Expr     -- true when first expr is greater than second.
              deriving (Eq, Show)



b1, b2, bEqual, bGreater :: BoolExpr
bEqual = Equal (Op Sub (Lit 10) (Lit 5)) (Op Add (Lit 8) (Lit (-3)))
bGreater = Greater (Lit 33) (Lit 14)
b1 = And bEqual bGreater
b2 = Not (Greater (Lit 20) (Op Mul (Lit 3) (Lit 6)) )

e1, e2 :: Expr
e1 = Op Add (Lit 6) (Op Div (Op Mod (Lit 146) (Lit 32)) (Lit 6))
e2 = Op Sub (Op Mul (Lit 4) (Lit 3)) (Op Add (Lit 10) (Lit 3))
e3 = If b1 e1 e2
e4 = If b2 e1 e2




bEval :: BoolExpr -> Bool
bEval (BoolLit b) = b
bEval (And b1 b2) = (bEval b1) && (bEval b2)
bEval (Not b) = not (bEval b)
bEval (Equal e1 e2) = (eval e1) == (eval e2)
bEval (Greater e1 e2) = (eval e1) > (eval e2)


eval :: Expr -> Integer
eval (Lit n) = n
eval (If bExp e1 e2) = if (bEval bExp) then (eval e1) else (eval e2)
eval (Op Add e1 e2) = (eval e1) + (eval e2)
eval (Op Sub e1 e2) = (eval e1) - (eval e2)
eval (Op Mul e1 e2) = (eval e1) * (eval e2)
eval (Op Mod e1 e2) = (eval e1) `mod` (eval e2)
eval (Op Div e1 e2)
    | sndExprNum /= 0 = (eval e1) `div` sndExprNum
    | otherwise = error ("Divide by zero: " ++ (showExpr e2))
    where sndExprNum = (eval e2)



--------------------------------------------------------

showExpr :: Expr -> String
showExpr expr = sho expr 0

sho :: Expr -> Int -> String
sho (Lit n) c = show n
sho (Op Add e1 e2) c = applySho c (sho e1 (c+1) ++ " + " ++ sho e2 (c+1))
sho (Op Sub e1 e2) c = applySho c (sho e1 (c+1) ++ " - " ++ sho e2 (c+1))
sho (Op Mul e1 e2) c = applySho c (sho e1 (c+1) ++ " * " ++ sho e2 (c+1))
sho (Op Div e1 e2) c = applySho c (sho e1 (c+1) ++ " / " ++ sho e2 (c+1))
sho (Op Mod e1 e2) c = applySho c (sho e1 (c+1) ++ " % " ++ sho e2 (c+1))
sho (If b e1 e2) c = "IF " ++ shoB b c ++ "\nTHEN " ++ applySho c (sho e1 (c+1))
                    ++ "\nELSE " ++ applySho c (sho e2 (c+1))
-- NOTE add: | If BoolExpr Expr Expr

applySho :: Int -> String -> String
applySho count exprStr
    | count == 0 = exprStr
    | otherwise = applyOuterParens exprStr

--------------------------------------------------------

showBExpr :: BoolExpr -> String
showBExpr bExpr = shoB bExpr 0

shoB :: BoolExpr -> Int -> String
shoB (BoolLit n) c = show n
shoB (And b1 b2) c = applyShoB c (shoB b1 (c+1) ++ " && " ++ shoB b2 (c+1))
shoB (Not b) c = applyShoB c ("NOT " ++ shoB b (c+1))
shoB (Equal e1 e2) c = applySho c (sho e1 (c+1) ++ " == " ++ sho e2 (c+1))
shoB (Greater e1 e2) c = applySho c (sho e1 (c+1) ++ " > " ++ sho e2 (c+1))

applyShoB :: Int -> String -> String
applyShoB count bexprStr
    | count == 0 = bexprStr
    | otherwise = applyOuterParens bexprStr

------------------------------
applyOuterParens :: String -> String
applyOuterParens s = "(" ++ s ++ ")"

----------------------------------------------------------

printExpr :: Expr -> IO()
printExpr e = putStrLn $ showExpr e

printBExpr :: BoolExpr -> IO()
printBExpr b = putStrLn $ showBExpr b