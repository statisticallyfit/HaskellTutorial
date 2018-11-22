
data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr deriving (Eq, Show)


-- exercise 4 -------------------------------------------------------------------------

-- note counts number of operators in an expression
size :: Expr -> Integer
size (Lit n) = 0
size (Add e1 e2) = 1 + size e1 + size e2
size (Sub e1 e2) = 1 + size e1 + size e2

---------------------------------------------------------------------------------------




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