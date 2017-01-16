{-# LANGUAGE FlexibleContexts #-}
module Types where


-- import Util.Specific
-- import Util.General


{-import Test.QuickCheck -- (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes-}
import Control.Monad hiding (join)
import Control.Applicative
import Data.Char
import Numeric -- math library
import Data.Maybe
import Data.List
-- import Data.List.Unique (unique) -- just use nub
import Data.Ratio hiding (show)



data Function a
    = Sin a | Cos a | Tan a |
      Csc a | Sec a | Cot a |
      Arcsin a | Arccos a | Arctan a |
      Arccsc a | Arcsec a | Arccot a |
      Sinh a | Cosh a | Tanh a |
      Csch a | Sech a | Coth a |
      Arcsinh a | Arccosh a | Arctanh a |
      Arccsch a | Arcsech a | Arccoth a |
      Ln a | E a | Log a a -- first expr is base
    deriving (Eq)

data Op = AddOp | SubOp | MulOp | DivOp | PowOp deriving (Eq)

type RationalNum = Ratio Int
data Fraction = Rate RationalNum deriving (Eq)

data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
    | Pow Expr Expr | Neg Expr | Num Int | Frac Fraction | Var String | F (Function Expr)
    deriving (Eq)

type Coeff = Int
type Description = (Expr, Expr, Expr)

data Code = Poly [Fraction] | Trig [Description] | InvTrig [Description]
    | Hyperbolic [Description] | InvHyp [Description] | Logarithmic [Description]
    deriving (Eq, Show)





instance Num Fraction where
    negate (Rate ratio) = Rate $ negate ratio
    (Rate r1) + (Rate r2) = Rate $ r1 + r2 -- liftA2 (+)
    (Rate r1) * (Rate r2) = Rate $ r1 * r2
    fromInteger num = Rate $ (fromInteger num) % 1
    abs (Rate ratio) = Rate $ abs ratio
    signum (Rate ratio) = Rate $ signum ratio

instance Ord Fraction where
    compare (Rate r1) (Rate r2) = compare r1 r2



instance Show Fraction where
    show (Rate ratio)
        | numerator ratio == 0 = show 0
        | denominator ratio == 1 = show (numerator ratio)
        | otherwise = (show (numerator ratio)) ++ "/" ++ (show (denominator ratio))

instance Functor Function where
    fmap f (Sin x) = Sin (f x)
    fmap f (Cos x) = Cos (f x)
    fmap f (Tan x) = Tan (f x)
    fmap f (Csc x) = Csc (f x)
    fmap f (Sec x) = Sec (f x)
    fmap f (Cot x) = Cot (f x)
    fmap f (Arcsin x) = Arcsin (f x)
    fmap f (Arccos x) = Arccos (f x)
    fmap f (Arctan x) = Arctan (f x)
    fmap f (Arccsc x) = Arccsc (f x)
    fmap f (Arcsec x) = Arcsec (f x)
    fmap f (Arccot x) = Arccot (f x)
    fmap f (Sinh x) = Sinh (f x)
    fmap f (Cosh x) = Cosh (f x)
    fmap f (Tanh x) = Tanh (f x)
    fmap f (Csch x) = Csch (f x)
    fmap f (Sech x) = Sech (f x)
    fmap f (Coth x) = Coth (f x)
    fmap f (Arcsinh x) = Arcsinh (f x)
    fmap f (Arccosh x) = Arccosh (f x)
    fmap f (Arctanh x) = Arctanh (f x)
    fmap f (Arccsch x) = Arccsch (f x)
    fmap f (Arcsech x) = Arcsech (f x)
    fmap f (Arccoth x) = Arccoth (f x)
    fmap f (E x) = E (f x)
    fmap f (Ln x) = Ln (f x)
    fmap f (Log base x) = Log (f base) (f x)


instance Show Op where
    show AddOp = "(+)"
    show SubOp = "(-)"
    show MulOp = "(*)"
    show DivOp = "(/)"
    show PowOp = "(^)"


-- TODO idea: count num elements and then decide whether ot put brackets.
-- Example: x^6 * 4 is shown as 4x^6 while 4 * (x+3) is shown as 4(x+3)
-- idea: glued things are wrapped each.
-- NOTE original
instance Show Expr where
    show (Var x) = x
    show (Num n) = show n
    show (Frac fraction) = show fraction
    show (Neg (Num n)) = if (n < 0) then ("-(" ++ show n ++ ")") else ("-" ++ show n)
    show (F func) = show func
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2)
        | isSeparable e2 = show e1 ++ " - [" ++ show e2 ++ "]"
        | otherwise = show e1 ++ " - " ++ show e2

    show (Mul (Num n) (F f)) = show n ++ show f
    show (Mul frac@(Frac (Rate n)) (F f))
        = if (denominator n == 1)
        then (show frac ++ show f)
        else ("(" ++ show frac ++ ")" ++ show f)
    show (Mul (Frac frac@(Rate n)) other)
        = if (denominator n == 1)
        then (show frac ++ show other)
        else ("(" ++ show frac ++ ")" ++ show other)
    show (Mul p@(Pow _ _) f@(F _)) = "(" ++ show p ++ ")" ++ show f
    show (Mul (Num n) p@(Pow _ _)) = show n ++ show p
    show (Mul np@(Mul (Num n) p@(Pow _ _)) other) = "(" ++ show np ++ ")(" ++ show other ++ ")"
    show (Mul (Mul other p@(Pow _ _)) f@(F _))
        | many other = "(" ++ show other ++ ")(" ++ show p ++ ")" ++ show f
        | otherwise = show other ++ "(" ++ show p ++ ")" ++ show f
        where
        many e = (not $ isMono e) || (length $ splitAS e) > 1 || (length $ split MulOp e) > 1
    show (Mul (Mul (Num n) pow@(Mul b (Pow _ _))) c)
        = "(" ++ show n ++ ")" ++ show pow ++ "(" ++ show b ++ ")"
    show (Mul (Mul a pow@(Mul b (Pow _ _))) c) = show a ++ show pow ++ "(" ++ show b ++ ")"
    show (Mul other (F f))
        | many other = "(" ++ show other ++ ")" ++ show f
        | otherwise = show other ++ show f
        where many e = isSeparable e || isMono e || isPow e
    show (Mul e1 ng@(Neg (Num n)))
        = if (n >= 0)
        then (show e1 ++ "(-" ++ show n ++ ")")
        else (show e1 ++ "(-(-" ++ show (-1*n) ++ "))")
    -- HELP next few cases are trying to envelop the add expr in brackets:
    -- let e = (Num 3 .* Num 4 .* x .^ Num 7 .+ x .^ Num (-8)) .* (F (Tan x))
    show (Mul e1@(Add _ _) e2) = "(" ++ show e1 ++ ")" ++ e2'
        where
        many e = isPow e || isSeparable e
        e2' = if (many e2) then ("(" ++ show e2 ++ ")") else (show e2)
    show (Mul e1 e2@(Add _ _)) = e1' ++ "(" ++ show e2 ++ ")"
        where
        many e = isPow e || isSeparable e
        e1' = if (many e1) then ("(" ++ show e1 ++ ")") else (show e1)
    show (Mul (Mul e1@(Add _ _) e2) e3) = "(" ++ show e1 ++ ")" ++ e2' ++ e3'
        where
        many e = isPow e || isSeparable e
        e2' = if (many e2) then ("(" ++ show e2 ++ ")") else (show e2)
        e3' = if (many e3) then ("(" ++ show e3 ++ ")") else (show e3)
    show (Mul (Mul e1 e2@(Add _ _)) e3) = e1' ++ "(" ++ show e2 ++ ")" ++ e3'
        where
        many e = isPow e || isSeparable e
        e1' = if (many e1) then ("(" ++ show e1 ++ ")") else (show e1)
        e3' = if (many e3) then ("(" ++ show e3 ++ ")") else (show e3)
    show (Mul e1 e2)  -- = "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"
        | many1 && many2 = "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"
        | many1 = "(" ++ show e1 ++ ")" ++ show e2
        | many2 = show e1 ++ "(" ++ show e2 ++ ")"
        | otherwise = show e1 ++ show e2
        where
        many1 = isPow e1 || isSeparable e1
        many2 = isPow e2 || isNum e2 || isSeparable e2 || isNeg e2

    show (Div (Num n) (Num m)) = show n ++ "/" ++ show m
    show (Div (Frac f1) (Frac f2)) = "(" ++ show f1 ++ ")/(" ++ show f2 ++ ")"
    show (Div e1 e2)
        | few e1 && few e2 = surround $ show e1 ++ "/" ++ show e2
        | few e1 = surround $ show e1 ++ "/(" ++ show e2 ++ ")"
        | few e2 = surround $ "(" ++ show e1 ++ ")/" ++ show e2
        | otherwise = surround $ "(" ++ show e1 ++ ") / (" ++ show e2 ++ ")"
        where
        surround eStr = "{" ++ eStr ++ "}"
        few e = (isVar e || isNum e || (not $ isMono e))
            && ((not $ isNumOrFracNeg e) && (not $ isNegNumOrFrac e)
            && (hasOnlyOneFunction e) && ((length $ split MulOp e) == 1))

    show (Pow e1 (Num n))
        | few e1 && (n < 0) = show e1 ++ "^(" ++ show n ++ ")"
        | few e1 = show e1 ++ "^" ++ show n
        | n < 0 = "(" ++ show e1 ++ ")^(" ++ show n ++ ")"
        | n > 0 = "(" ++ show e1 ++ ")^" ++ show n
        | otherwise = show e1 ++ "^" ++ show n
        where
        few e = isVar e || isNum e || hasNeg e
    show (Pow e1 neg@(Neg (Num n)))
        | few e1 = show e1 ++ "^(" ++ show neg ++ ")"
        | otherwise = "(" ++ show e1 ++ ")^(" ++ show neg ++ ")"
        where
        few e = isVar e || isNum e || hasNeg e
    show (Pow x d@(Div (Num a) (Num b))) = show x ++ "^(" ++ show d ++ ")"
    show (Pow x d@(Div e1 e2)) = show x ++ "^" ++ show d
    show (Pow e1 e2)
        | isSeparable e1 && isSeparable e2 = "(" ++ show e1 ++ ")^(" ++ show e2 ++ ")"
        | isSeparable e1 = "(" ++ show e1 ++ ")^" ++ show e2
        | isSeparable e2 = show e1 ++ "^(" ++ show e2 ++ ")"
        | otherwise = show e1 ++ "^" ++ show e2

    show (Neg m@(Mul _ _)) = "-" ++ show m
    show (Neg d@(Div _ _)) = "-" ++ show d
    show (Neg p@(Pow _ _)) = "-" ++ show p
    show (Neg f@(F _)) = "-" ++ show f
    show (Neg (Var x)) = "-" ++ x
    show (Neg e) = "-(" ++ show e ++ ")"


--Mul (Add (Mul (Mul (Num 3) (Num 4)) (Pow (Var "x") (Num 7))) (Pow (Var "x") (Num (-8)))) (F (Tan x))

-- TODO how to fmap this?
instance Show a => Show (Function a) where
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
    show (Sinh e) = "sinh(" ++ show e ++ ")"
    show (Cosh e) = "cosh(" ++ show e ++ ")"
    show (Tanh e) = "tanh(" ++ show e ++ ")"
    show (Csch e) = "csch(" ++ show e ++ ")"
    show (Sech e) = "sech(" ++ show e ++ ")"
    show (Coth e) = "coth(" ++ show e ++ ")"
    show (Arcsinh e) = "arcsinh(" ++ show e ++ ")"
    show (Arccosh e) = "arccosh(" ++ show e ++ ")"
    show (Arctanh e) = "arctanh(" ++ show e ++ ")"
    show (Arccsch e) = "arccsch(" ++ show e ++ ")"
    show (Arcsech e) = "arcsech(" ++ show e ++ ")"
    show (Arccoth e) = "arccoth(" ++ show e ++ ")"
    show (E e) = "e^(" ++ show e ++ ")"
    show (Ln e) = "ln(" ++ show e ++ ")"
    show (Log b a) = "log" ++ show b ++ "(" ++ show a ++ ")"


x = Var "x"
y = Var "y"
z = Var "z"
t = Var "t"
s = Var "s"
a = Var "a"
b = Var "b"

infixl 6 .+
infixl 6 .-
infixl 7 .*  -- NOTE made this right associative so tree would lean to the left (go right down)
infixl 7 ./
infixl 8 .^

(.+), (.-), (.*), (./), (.^) :: Expr -> Expr -> Expr
(.+) = Add
(.-) = Sub
(.*) = Mul
(./) = Div
(.^) = Pow






--- TODO: rename to be toFraction
makeFraction :: Int -> Fraction
makeFraction n = Rate $ n % 1


isSeparable :: Expr -> Bool
isSeparable expr = (length $ splitAS expr) > 1
