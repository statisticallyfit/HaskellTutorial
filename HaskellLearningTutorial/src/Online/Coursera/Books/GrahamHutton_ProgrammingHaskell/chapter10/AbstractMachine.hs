

data Expr = Val Int
          | Add Expr Expr
          | Mult Expr Expr
          deriving (Eq, Ord, Show)
{-

value           :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y
-}


e1 :: Expr
e1 = Add (Add (Val 2) (Val 3)) (Val 4)

e2 :: Expr
e2 = Add (Val 2) (Add (Val (-8)) (Val (-3)) )

e3 :: Expr
e3 = Add (Val 3) (Mult (Val 2) (Val 6))


-- making order of operations explicit with control stack
-- control stack: list of operations to be performed after current evaluation.
type Cont = [Op]
data Op = EVAL_A Expr | ADD Int | EVAL_M Expr | MULT Int

-- evaluate expression in context of control stack
-- note: if expression is integer it is already evaluated so execute the control stack.
-- note: else evaluate first arg x and put operation EVAL y on top of control stack
-- to show that y should be evaluated once x is completed.
eval              :: Expr -> Cont -> Int
eval (Val n) c    = exec c n
eval (Add x y) c  = eval x (EVAL_A y : c)
eval (Mult x y) c = eval x (EVAL_M y : c)

-- note if stack is empty, return integer
-- If top of stack is Eval y then evaluate y and put ADD n on top of stack to evaluate after.
-- If top is ADD n the evaluation of two args is now complete and exec remaining
-- control stack in context of the sum of the two resulting integer values.
exec                  :: Cont -> Int -> Int
exec [] n             = n
exec (EVAL_A y : c) n = eval y (ADD n : c)
exec (EVAL_M y : c) n = eval y (MULT n : c)
exec (ADD n : c) m    = exec c (n + m)
exec (MULT n : c) m   = exec c (n * m)


-- note: evaluates expr to int by invoking eval with given expr and empty control stack.
value   :: Expr -> Int
value e = eval e []



main = do
    print e1; print $ value e1
    putStrLn ""
    print e2; print $ value e2
    putStrLn ""
    print e3; print $ value e3 -- exercise 7