module Testlog where


data Expr = Var String | Num Integer deriving (Eq, Show)

data Logarithm = Log Expr Expr deriving (Eq, Show)

instance Functor Logarithm where
    fmap f (Log b e) = Log (f b) (f e)


instance Num Expr where
    negate (Var x) = Var ("-" ++ x)
    negate (Num n) = Num (-n)

    (Var x) + (Var y) = Var $ x ++ " + " ++ y
    (Num n1) + (Num n2) = Num $ n1 + n2

    (Var x) * (Var y) = Var $ x ++ y
    (Num n1) * (Num n2) = Num $ n1 * n2

    -- fromInteger num = Var $ show num
    fromInteger num = Num num

    abs (Var x) = Var "x"
    abs (Num n) = Num $ abs n

    signum (Var x) = Var $ if length x > 1 then "-1" else "1"
    signum (Num n) = Num $ signum n

addLogs :: Num a => Logarithm -> Logarithm -> Logarithm
addLogs (Log b x) (Log c y)
    | b == c = Log b (x + y )
    | otherwise = error "bases aren't equal"



x = Var "x"
y = Var "y"


main = do
    print $ addLogs (Log (Num 2) x) (Log (Num 2) y)