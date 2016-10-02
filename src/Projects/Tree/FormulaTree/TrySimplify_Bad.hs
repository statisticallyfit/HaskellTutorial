infixl 4 :+:
infixl 5 :*:, :/:
infixr 6 :^:

data Function
    = Sin Expr | Cos Expr | Tan Expr |
      Csc Expr | Sec Expr | Cot Expr |
      Arcsin Expr | Arccos Expr | Arctan Expr |
      Arccsc Expr | Arcsec Expr | Arccot Expr
    deriving (Eq)


data Expr = Var Char
             | Const Int
             | (Expr) :+: (Expr)
             | Expr :-: Expr
             | (Expr) :*: (Expr)
             | (Expr) :^: (Expr)
             | (Expr) :/: (Expr)
             | F Function
             deriving (Eq)


instance Show Expr where
    show (Var x) = [x]
    show (F func) = show func
    show (e1 :+: e2) = show e1 ++ " + " ++ show e2
    show (e1 :-: e2) = show e1 ++ " - " ++ show e2
    show (e1 :*: e2) = show e1 ++ show e2
    show (e1 :/: e2) = show e1 ++ " / " ++ show e2
    show (e1 :^: e2) = show e1 ++ "^" ++ show e2
    show (Const n) = show n

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




e1 :: Expr
e1 = Const 3 :*: Var 'x' :^: Const 2 --3x^2
e2 = Const(4) :*: Var 'x' :*: (F (Sin (Var 'x'))) :*: Const(3) :*: (F (Cos (Var 'x')))
e3 = Const(2) :*: Var 'x' :+: Const(7) :*: Var 'x'
e4 = e2 :+: e3
e5 = Const(4) :*: Var 'x' :+: Const(3) :*: Var 'x' :^: Const(5)


simplify :: (Num a, Eq a, Floating a) => Expr -> Expr
simplify (Const a :+: Const b) = Const (a + b)
simplify (a       :+: Const 0) = simplify a
simplify (Const 0 :+: a      ) = simplify a

simplify (Const a :*: Const b) = Const (a*b)
simplify (a :*: Const 1)         = simplify a
simplify (Const 1 :*: a)         = simplify a
simplify (a :*: Const 0)         = Const 0
simplify (Const 0 :*: a)         = Const 0

simplify (Const a :^: Const b)       = Const (a ^ b)
simplify (a :^: Const 1)             = simplify a
simplify (a :^: Const 0)             = Const 1
simplify ((c :^: Const b) :^: Const a) = c :^: (Const (a*b))

simplify (Const a :*: (Const b :*: expr)) = (Const $ a*b) :*: (simplify expr)
simplify (Const a :*: expr :*: Const b) = (Const $ a*b) :*: (simplify expr)
simplify (expr :*: Const a :*: Const b) = (Const $ a*b) :*: (simplify expr)
simplify (Const a :*: (b :+: c))        = (Const a :*: (simplify b)) :+: (Const a :*: (simplify c))

simplify (Const 0 :/: a        ) = Const 0
simplify (Const a :/: Const 0)   = error "Division by zero!"
simplify (Const a :/: Const b)   | a == b = Const 1 -- only when a == b
simplify (a       :/: Const 1)   = simplify a

simplify (a :/: b)  = (simplify a) :/: (simplify b)
simplify (a :^: b)  = (simplify a) :^: (simplify b)
simplify (a :*: b)  = (simplify a) :*: (simplify b)
simplify (a :+: b)  = (simplify a) :+: (simplify b)
simplify x          = id x

fullSimplify expr = fullSimplify' expr (Const 0) -- placeholder
    where fullSimplify' cur last | cur == last = cur
                                 | otherwise = let cur' = simplify cur
                                               in fullSimplify' cur' cur




