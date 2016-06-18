
data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr deriving (Eq, Show)

expr1, expr2, expr3, expr4, expr5, expr6 :: Expr
expr1 = (Add (Sub (Lit 3) (Lit 1)) (Lit 3))
expr2 = (Add (Lit 67) (Lit (-34)))


{-
NOTE evaluations

eval (Add (Sub (Lit 3) (Lit 1)) (Lit 3))
= eval (Sub (Lit 3) (Lit 1))    +    eval (Lit 3)
= (eval (Lit 3) - eval (Lit 1)) + eval (Lit 3)
= (3 - 1) + 3

showExpr (Add (Lit 67) (Lit (-34)))
= showExpr (Lit 67) ++ " + " ++ showExpr (Lit (-34))
= "(67 + -34)"
-}



eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)



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