{-# LANGUAGE FlexibleContexts #-}
module ExprPlan where

{-import Test.QuickCheck -- (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes-}
import Control.Monad hiding (join)
import Control.Applicative
import Data.Char
import Numeric -- math library
import Data.Maybe
import Data.List
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

instance Show Fraction where
    show (Rate ratio)
        | numerator ratio == 0 = show 0
        | denominator ratio == 1 = show (numerator ratio)
        | otherwise = (show (numerator ratio)) ++ "/" ++ (show (denominator ratio))

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
        many2 = isPow e2 || isNum e2 || isSeparable e2

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

---------------------------------------------------------------------------------------------

e1 :: Expr
e1 = Num(4) .* x .* (F (Sin x)) .* Num(2) .* (F (Cos x)) .* Num(5) .* Num(2) .* Num(3) .* (F (Tan x))
e2 = e1 .+ Num(2) .* x .+ Num(7) .* x
e3 = Num(4) .* x .+ Num(3) .* x .^ Num(5) .- (F (Sin (Num(3) .+ x)))
e4 = Neg(F (Sin x)) .* Neg(Num(2))
e5 = Neg (Num(4) .* x .+ Num(2) .* y)
e6 = Num (-7) .* x .^ Num 2 .+ Num 3 .* x .+ Num 4 .* x .+ Num 5 .* x .^ Num 2 .-
    Num 3 .* (F $ Sin $ Num 4 .* x) .+ Num 5 .* (F $ Cos x) .+ Num 2 .+ Num 3
e7 = (Num 3 .* x .^ Num 3) ./ (Num 3 .* x .^ (Num 1 ./ Num 3)) .-
    (Num 8 .* x .^ Num 9) ./ (Num 4 .* x .^ Num 3)
e7' = (Num 3 .* x .^ Num 3) ./ ((Num 3 .* x .^ (Num 1 ./ Num 3)) .-
    (Num 8 .* x .^ Num 9)) ./ (Num 4 .* x .^ Num 3)
e8 = e6 .+ e7
-- NOTE IMPORTANT you must put (e1 * e2) / (e3 * e4) brakcets like that because otherwise
-- error says you cannot mix ./ and .* because one is infix l and other is infix r.
e9 = ((Num 3 .* x .^ Num 3) ./ (Num 3 .* x .^ (Num 1 ./ Num 3))) .*
     ((Num 8 .* x .^ Num 9) ./ (Num 4 .* x .^ Num 3))
e10 = (Num 3 .* x .^ Num 3) ./ ((Num 3 .* x .^ (Num 1 ./ Num 3)) .*
     (Num 8 .* x .^ Num 9)) ./ (Num 4 .* x .^ Num 3)
e11 = Num 4 .* (x .+ Num 3)
-- TODO fix show for this one using numTerms.
e12 = ((F (Sin x)) .+ (F (Cos x)) .* Num 4 .* x .^ Num 2) .^ (x .+ Num 5 .- (F (Tan (Num 2 .* x))))
e13 = Num 4 .* x .+ Num 3 .* x .+ x .+ Num 6 .* x .^ Num 7 .+ Num 2 .+ Num 3
e14 = Num 10 .* x .- (F (Sin x)) .- (F (Cos (Num 2 .* x))) .+ x .- y .- Num 5 .- Num 8 .- Num 9 .+ x
e15 = F (Sin (x .+ Num 3 .+ Num 4 .+ Num 8 .* x .- y .+ Num 2 .^ x))
e16 = Num 2 .* Num 8 .* Num 2 .* x .^ Num (-7) .* Num 3 .* x .* (F (Sin (x .^ Num (-8))))
e17 = Num 2 .* x .^ Num (-7) .* (Num 3 .+ x) .* (F (Sin (x .^ Num (-8))))
-- NOTE to test poly encoding and decoding
e18 = ( ( (x .+ Num 1) .^ Num 3) ./ (x .+ Num 1))
e19 = ( ( (x .+ Num 1) .^ Num 3) ./ ((x .+ Num 1) .^ Num 4))
e20 = ( ( (x .+ Num 1) .^ Num (-3)) ./ ((x .+ Num 1) .^ Num 4))
e21 = Num 3 .* x .^ Num 2 .* Num 4 .* x .^ Num (-3) .+ Num 5 .* x .-
    ( ( (x .+ Num 1) .^ Num 3) ./ (x .+ Num 2)) .+ (x .+ Num 1) .^ Num 3 .+
    Num 5 .* x .^ Num 3 .* Num 6 .* x .^ Num 2 .- ((Num 4 .* x) ./ (x .+ Num 1))
e22 = (Num 3 .* x .^ Num (-2) .* Num 4 .* x .^ Num 3 .+ Num 5 .* x .^ Num 4 .* (x .+ Num 1) .^ Num 6) ./
    (x .+ Num 1)
e23 = (Num 3 .* x .^ Num (-2) .* Num 4 .* x .^ Num 3 .+ Num 5 .* x .^ Num 4 .* (x .+ Num 1) .^ Num 6) ./
    (Num 8 .* x .^ Num 3) .* (F (Sin (Num 2 .* x)))
e24 = (Num 3 .* x .^ Num (-2) .* Num 4 .* x .^ Num 3 .+
    Num 3 .* x .^ Num (-8) .* Num 5 .* x .^ Num 4 .* (x .+ Num 1) .^ Num 6) ./
    (Num 8 .* x .^ Num 3)  .* (F (Tan (Num (-3) .* x .^ Num 2)))
-- note test unjoin polyfunc
e25 = Num 4 .* x .^ Num 7 .* x .* Num 8 ./ (Num 4 .* x .* F (Sin x)) .^ Num 4 .* x .* Num 3
e26 = Num 4 .* x .^ Num 7 .* x .* Num 8 ./ (Num 4 .* x) .* F (Sin x) .^ Num 4 .* x .* Num 3
-- note test unjoiner and codifypolyfunc
e27 = (Num 4 .* x .+ Num 5 .* x .* Num 6 .* x .^ Num 8) ./
    (Num 5 .* x .* ((Num 4 .* x .+ Num 3 .* F (Sin x)) .^ Num 7))

-- testing meltpolyfunc
pf1 = (Num 4 .* x  .- Num 5 .* x .^ (x ./ (Num 3 .+ Num 2))) .* (F (Sin x)) .* Num 8 .* x
pf2 = (Num 4 .* x .+ Num 33 .* x .^ Num (-8)) ./ (Num 4 .* x .* F (Cos x) .+ Num 9 .* x)
pf3 = (Num 4 .* x .* F (Cos x) .+ Num 9 .* x) ./ (Num 4 .* x .+ Num 33 .* x .^ Num (-8))
pf4 = (Num 4 .* (x .* F (Cos x)) .^ Num 11 .+ Num 9 .* x) ./ (Num 4 .* x .+ Num 33 .* x .^ Num (-8))
pf5 = (Num 4 .* x .+ Num 33 .* x .^ Num (-8)) ./ (Num 4 .* (x .* F (Cos x)) .^ Num 11 .+ Num 9 .* x)
pf6 = (Num 4 .* x .- Num 33 .* x .^ Num (-8)) ./ (Num 4 .* (x .* F (Cos x)) .^ Num 11 .+ Num 9 .* x)
pf7 = (Num 4 .* x .- Num (-33) .* x .^ Num (-8)) ./ (Num 4 .* (x .* F (Cos x)) .^ Num 11 .+ Num 9 .* x)

e = x .* Num 3 .* Num 2 .* x .^ Num 9 .* (F (Sin x)) .+
    Num (-2) .* x .* Num 8 .* ((F (Sin x)) .^ (x .^ Num 2)) .-
    (Num 2 .* x .* Num 8 .* ((F (Sin x)) .^ (x .^ Num 2)) .-
    Num 2 .* Num 7 .* x .* Num 3 .* Num 2 .* x .^ Num 9 .* (F (Sin x)) .-
    (Num 8 .* F (Cos x) .-
    (Num 3 .+ x) .* (F (Cos x)) ) )

-- testing meltpoly func how it does with inner functions inside powers.
pfhard = (x .^ Num 2) ./ (Num 5 .* x .* (Num 4 .* x .+ F (Sin x)) .^ Num 7)
pfharder = (x .^ Num 2) ./ (Num 5 .* x .* (Num 4 .* x .+ (F (Sin x) .- Num 8) .^ Num 22) .^ Num 7)


-- testing addCodes with adder function

ys1 = [(Num 4,x,x .^ Num 2), (Num 6, Num 5, x .^ Num 3), (Num (-10),x,x .^ Num 2),
    (Num 2, x, x .^ Num 2), (x .+ Num 1 , Num 5, x .^ Num 3), (Num 22, Num 5, x .^ Num 3)]

ys2 = [(Num (-8),x,x .^ Num 2), (x .^ Num 7 .+ Num 3 .* x .^ Num 2, Num 5, x .^ Num 3),
    (Num 4, Num 4, x .^ Num 4), (Num 2, x, x .^ Num 2), (Num 1,Num 5,x .^ Num 3),
    (Num 1, Num 4, x .^ Num 4)]

ys3 = [(Num (-20),x,x .^ Num 2), (Num 5, Num 5, x .^ Num 3), (Num 2, Num 5, x .^ Num 3),
    (Num 1,Num 4, x .^ Num 4), (Num (-2),Num 4, x .^ Num 4), (Num 19, Num 5, x .^ Num 3)]

ys4 = [(Num 88, x, x .^ Num 2), (Num 90 .+ x .^ Num 3 .- Num 3 .* x .^ Num 33, x, x .^ Num 2),
    (Num 44, x, x .^ Num 2), (Num 11 .+ x .- x .^ Num 6, Num 5, x .^ Num 3),
    (Num 12, Num 4, x .^ Num 4), (Num 5, Num 5, x .^ Num 3)]

ys5 = [(Num 7,x,x .^ Num 2), (Num (-54), Num 5, x .^ Num 3), (Num 14, Num 4, x .^ Num 4),
    (Num 44, Num 4, x .^ Num 4), (Num 3, Num 4, x .^ Num 4), (Num (-8) .* x, Num 5, x .^ Num 3)]

ys = concat $ [map Trig [ys1, ys2, ys3, ys4, ys5]] ++ [map InvHyp [ys3, ys5, ys2]] ++
    [map Hyperbolic [ys1,ys2, ys5, ys4]] ++ [map Logarithmic [ys5, ys3, ys2, ys1, ys4]] ++
    [map Trig [ys1, ys5, ys3]]  ++ [map Logarithmic [ys2, ys4, ys2, ys4]] ++
    [map InvHyp [ys1, ys1, ys2, ys3, ys4, ys4, ys1]]

--- testing meltExpon
e28 = (Pow (Neg (Num 3)) (x .+ Num 1 .+ Num 3 .+ Num 2 .* x))
---------------------------------------------------------------------------------------------


{-
TODO PLAN OVERALL * ~ *

input -> split Add and Sub -> partition -> (poly terms, functions)

poly terms -> send to addpoly

functions -> partition
	-> type 1: x^2 sin(x) - send to addSingleFunctions
	-> type 2: sinxtanxcosx - send to function simplifyFunctions (or fold using
	simplifyFunctions principles)
type 1:
principle:
represent sin ^ tan x (x) or sin^ 2 (3x^6) as:
Trig [(tan x, X), 0,0,0,0,0] and,
Trig [(Num 2, (3x^6)),0,0,0,0,0] respectively.

principle: we add only if both elements in tuple are equal pairwise, (same for sub) resulting in
new data holder: Trig [(Num 2, same exp, same arg)]

and we multiply only if the argument is equal (snd) so we get Trig [(tanx + Num 2 as exp, X)]
assuming arg was X for both. same for div

---

Clean up with sweep Num
ALSO: order: distribute, negExplicit, then clean the simplified expr and input to simplify function
order for melt polyfunc (so we dont end up with -(16 x + 16x) when in fact we need (-16x + 16x)
=> negExplicit then distribute then clean.


** Also make function called rearrange or organize that transforms:
1 * sin x * cos x * 2 * 4 * ln x * 8 * (-x)
into
-64x * sinx * cos x * ln x

By:
assigning priorities to things:
num/negnum/numneg = priority 0 (highest)
var = priority 1
isMono = priority 2
(x + 1) ^ 2 or (x + 1) ( things like monomials that are nto quite) = priority 3
function = priority 4 (last)

-- precondition of function rearrange is: glued expr, so no add or sub (only as inner
like in x(x+1)(9)). Method: use chisel to get Div separate if present.

-}


splitAS :: Expr -> [Expr]
splitAS expr = concatMap (split SubOp) (split AddOp expr)


-- note must be used wisely or else order of operations won't be used.
-- for example:
-- split MulOp e5   ==>    [-(4),-x,-(2),-y] which is not correct
-- but we can do instead:
-- split SubOp e5   ==>    [-4x,-2y]
-- precondition: must take output from distribute (doesn't work on unflattened or unstacked exprs)
split :: Op -> Expr -> [Expr]
split _ (Var x) = [Var x]
split _ (Num n) = [Num n]
split _ (Frac f) = [Frac f]
split _ (F f) = [F f]
split _ p@(Pow _ _) = [p]
split op (Neg e) = handleNeg op (Neg e)
split op expr = pickMethod op expr

splitA :: Expr -> [Expr]
splitA e
    | isDiv e || isMul e = [e] -- TODO it's a mess here .. fix some other way.
    | isAdd e = splitAdd e
    | hasAdd e = init lefties ++ (newRight e lefties)
    | isNeg e = handleNeg AddOp e
    | not $ isVarNumFunc e = [e]
    | otherwise = split AddOp e
    where lefties = splitA (left e)
splitAdd (Add e1 e2)
    | notAdd e1 && notAdd e2 = [e1, e2]
    | notAdd e1 = [e1] ++ splitA e2
    | notAdd e2 = splitA e1 ++ [e2]
    | otherwise = splitA e1 ++ splitA e2
    where notAdd = not . hasAdd

splitS :: Expr -> [Expr]
splitS e
    | isSub e = splitSub e
    | hasSub e = init lefties ++ (newRight e lefties)
    | isNeg e = handleNeg SubOp e
    | not $ isVarNumFunc e = [e]
    | otherwise = split SubOp e
    where lefties = splitS (left e)
splitSub (Sub e1 e2)
    | notSub e1 && notSub e2 = [e1, Neg e2]
    | notSub e1 = [e1] ++ splitS (Neg e2)
    | notSub e2 = splitS e1 ++ [Neg e2]
    | otherwise = splitS e1 ++ map Neg (splitS e2)
    where notSub = not . hasSub

-- TODO -- think of how to splitM e11
-- TODO fix e12 tomorrow
splitM :: Expr -> [Expr]
splitM e
    | isMul e = splitMul e
    | isSeparable e = [e] -- note order of operations
    | isDiv e && hasMul e = if (not $ isMul expl) then [e] else (split MulOp expl)
    | hasMul e = init lefties ++ (newRight e lefties)
    | isNeg e = handleNeg MulOp e
    | not $ isVarNumFunc e = [e]
    | otherwise = split MulOp e
    where lefties = splitM (left e)
          expl = makeMulExplicit e
splitMul (Mul e1 e2)
    | notMul e1 && notMul e2 = [e1, e2]
    | notMul e1 = [e1] ++ splitM e2
    | notMul e2 = splitM e1 ++ [e2]
    | otherwise = splitM e1 ++ splitM e2
    where notMul = not . hasMul

-- NOTE no need to fix e9 so that it divides everywhere no?
splitD :: Expr -> [Expr]
splitD e
    | isDiv e = splitDiv e
    | isSeparable e = [e] -- note order of operations
    | isMul e && hasDiv e = if (not $ isDiv expl) then [expl] else (split DivOp expl)
    | hasDiv e = init lefties ++ (newRight e lefties)
    | isNeg e = handleNeg DivOp e
    | not $ isVarNumFunc e = [e]
    | otherwise = split DivOp e
    where lefties = splitD (left e)
          expl = makeDivExplicit e
splitDiv (Div e1 e2)
    | notDiv e1 && notDiv e2 = [e1, e2]
    | notDiv e1 = [e1] ++ splitD e2
    | notDiv e2 = splitD e1 ++ [e2]
    | otherwise = splitD e1 ++ splitD e2
    where notDiv = not . hasDiv




pickMethod :: Op -> Expr -> [Expr]
pickMethod AddOp expr = splitA expr
pickMethod SubOp expr = splitS expr
pickMethod MulOp expr = splitM {-$ makeMulExplicit-} expr
pickMethod DivOp expr = splitD {-$ makeDivExplicit-} expr

handleNeg :: Op -> Expr -> [Expr]
handleNeg op (Neg e)
    | isAdd e && (op == SubOp) = map Neg (pickMethod AddOp e)
    | isAdd e || isSub e = map Neg pick
    | isMul e || isDiv e || isPow e = [Neg (head pick)] ++ tail pick
    | otherwise = [Neg (head vnf)] ++ tail vnf
    where pick = pickMethod op e
          vnf = split op e -- stands for var or num or func

newRight :: Expr -> [Expr] -> [Expr]
newRight e ls
    | isAdd e = [(last ls) .+ foldl1 Add (split AddOp (right e))]
    | isSub e = [(last ls) .- foldl1 Sub (split SubOp (right e))]
    | isMul e = [(last ls) .* foldl1 Mul (split MulOp (right e))]
    | isDiv e = [(last ls) ./ foldl1 Div (split DivOp (right e))]
    | isPow e = [(last ls) .^ foldl1 Pow (split PowOp (right e))]
    | otherwise = [e]


---------------------------------------------------------

-- errors: TODO
-- 1) separate and differentiate different char args in the var constructor.
-- 2) represent RootPolys where pow = frac 1/3.
-- 3) handle each div
-- 4) handle non divs that need to be simplified: (x + 1)^3 (factor out when expo
-- is no bigger than 5, so 5 and under. If greater leave it as is.
-- 5) START HERE TOMORROW TODO: make polyroot instance of code and make all current
-- poly functions deal only with Nums not fractions, and polyroot to deal with fractions.
simplify :: Expr -> Expr
simplify (Pow base expo) = Pow (simplify base) (simplify expo)
simplify (F f) = F $ fmap simplify f
simplify expr = finishExpr $ ps' .+ fs' .+ ffs' .+ divs' .+ (rebuildAS other''')
    where
    -- TODO need to put distribute cases (sep)(sep)
    prepExpr = chisel . distribute . negExplicit . chisel
    finishExpr = chisel . negExplicit . distribute
    es = map chisel (splitAS (prepExpr expr))
    (ps, other) = partition isPoly es
    (fs, other') = partition (\e -> hasOnlyOneFunction e && (not $ isDiv e)) other
    (ffs, other'') = partition (\e -> hasManyFunctions e && (not $ isDiv e))  other'
    (divs, other''') = partition isDiv other''

    psI = if (null ps) then [Num 0] else ps
    fsI = if (null fs) then [Num 0] else fs
    ffsI = if (null ffs) then [Num 0] else ffs

    ps' = meltPoly (rebuildAS psI)
    fs' = meltPolyFunc (rebuildAS fsI)
    ffs' = meltFunctions (rebuildAS ffsI)
    divs' = rebuildAS $ map divSimplify divs


identifyNegDiv e = if isNeg e then (isDiv (getNeg e)) else False

expr = prepExpr e18
prepExpr = chisel . distribute . negExplicit . chisel
finishExpr = chisel . negExplicit . distribute
exprs = map chisel (splitAS expr)
(es, other0) = partition isExponential exprs
(ps, other1) = partition isPoly other0
(fs, other2) = partition (\e -> hasOnlyOneFunction e && (not $ isDiv e)) other1

(ffs, other3) = partition (\e -> hasManyFunctions e && (not $ isDiv e))  other2
(divs, other4) = partition (\e -> isDiv e || identifyNegDiv e) other3

esI = if (null es) then [Num 0] else es
psI = if (null ps) then [Num 0] else ps
fsI = if (null fs) then [Num 0] else fs
ffsI = if (null ffs) then [Num 0] else ffs

es' = meltExpon (rebuildAS esI)
ps' = meltPoly (rebuildAS psI)
fs' = meltPolyFunc (rebuildAS fsI)
ffs' = meltFunctions (rebuildAS ffsI)
divs' = rebuildAS $ map divSimplify divs


-- note expecting the remains from other'' in above function (must be div)
-- precondition: get one non-separable div expr at a time.
-- HELP goes into infinite loop ebcause of simplify at the front - FIX TODO
divSimplify :: Expr -> Expr
divSimplify (Neg expr) = Neg $ divSimplify expr
divSimplify expr = {-simplify-} (Div (simplify up) (simplify lo))
    where (Div up lo) = expr

------------------------------ Dealing with many functions ---------------------------------

-- note takes many sets of many funcs at a time. So expression can be separable.
-- note if expr is div (output from chisel) then we apply melt separately
-- postcondition returns separable function.
meltFunctions :: Expr -> Expr
meltFunctions (Num 0) = Num 0
meltFunctions e = e

-- note separates function part from other parts and then simplifies functions.
-- precondition: expr cannot be separable (has to be glued) so takes one set of many funcs at time.
simplifyFunctions :: Expr -> Expr
simplifyFunctions e = e

------------------------------ Dealing with Polynomials ---------------------------------

put :: Int -> a -> [a] -> [a]
put _ n [] = [n]
put index n xs
    | index < 0 = error "index is negative "
    | otherwise = front ++ [n] ++ (tail back)
    where (front, back) = splitAt index xs
          newBack = if null back then [] else (tail back)

elongate :: Int -> a -> [a] -> [a]
elongate len item xs = xs ++ (replicate (len - (length xs)) item)


toCommonDenom :: ((Int,Int), (Int,Int)) -> ((Int,Int), (Int,Int))
toCommonDenom ((n1,d1), (n2,d2)) = ((n1', lcm), (n2', lcm))
    where
    lcm = leastCommonMultiple d1 d2
    n1' = n1 * (lcm `div` d1)
    n2' = n2 * (lcm `div` d2)


-- note first arg doesn't have to be maybe just made it so that we can use it with foldl.
leastCommonMultiple :: Int -> Int -> Int
leastCommonMultiple a b = lcm a' b' c
    where
    (a', b') = (abs a, abs b)
    c = a' * b'
    lcm a b c
        | not (a == b) = if (a > b) then (lcm (a-b) b c) else (lcm a (b-a) c)
        | otherwise = fromInteger $ numerator $ toRational $ abs (c `div` a) -- convert to get int type


-- note gets the coefficient and power of the monomial.
-- precondition: input needs to be a mono
getCoefPowPair :: Expr -> (Fraction, Fraction)
getCoefPowPair (Var _) = (makeFraction 1, makeFraction 1)
getCoefPowPair (Neg e) = putNegFirst (getCoefPowPair e)
    where putNegFirst (n,p) = (-n, p)
getCoefPowPair expr = (coef, pow)
    where
    (cs, ps) = partition (\e -> not (isPow e || isVar e)) (split MulOp expr)
    pow = sum $ map getMakeFrac (map getPow ps)
    coef = if (null cs) then 1 else if (all isFrac cs) then (sum (map getFrac cs))
        else (makeFraction $ product (map getNum cs))
    getMakeFrac n = if (isFrac n) then (getFrac n) else (makeFraction $ getNum n)


[a,b] = splitAS expr
(ns, qs) = partition (\e -> not (isPow e || isVar e)) (split MulOp a)
pow = sum $ map getMakeFrac (map getPow qs)
coef = if (null ns) then 1 else if (all isFrac ns) then (sum (map getFrac ns))
    else (makeFraction $ product (map getNum ns))
getMakeFrac n = if (isFrac n) then (getFrac n) else (makeFraction $ getNum n)


makeFraction :: Int -> Fraction
makeFraction n = Rate $ n % 1


meltExpon :: Expr -> Expr
meltExpon (Pow constBase expo) = Pow constBase (simplify expo)


-- precondition: gets an expression with only polys
-- postcondition: returns simplified version
meltPoly :: Expr -> Expr
meltPoly expr
    | isNothing mCode = fromJust mExpr
    | otherwise = decodePoly (fromJust mCode)
    where
    (mCode, mExpr) = codifyPoly expr



-- note takes a single monomial and converts it to coded form
-- precondition: no neg powers, output of chisel, AND must be monomial.
codifyMono :: Expr -> Code
codifyMono expr = Poly $ zs ++ [c]
    where
    (c, p) = getCoefPowPair expr
    Rate p' = p
    numer = numerator p'
    zs = map makeFraction $ replicate numer 0



-- note takes list of polynomials and makes it into Group type Poly [...]
-- so 7x^2 + 3x^2 + 3x + 4x + 1 is [1, (3+4), (7+3)]
codifyPolyA :: Expr -> Code
codifyPolyA expr = foldl1 addPoly (map codifyPolyM ps)
    where ps = splitAS expr
          -- f p = if (isDiv p) then (codifyPolyD p) else


-- precondition: takes chisel output so has no negpowers. There is no division (assume) just mul.
-- Ignore expressions that are of form (x+1), just use the monomials.
-- testing there is always only one element in the array.
codifyPolyM :: Expr -> Code
codifyPolyM expr = foldl1 mulPoly (map codifyMono ps)
    where ps = split MulOp expr


-- TODO START HERE TOMORROW
-- precondition: takes chisel output which isDiv and which has no other div in the numerator
-- and denominator and no negpowers (that's why we can use codifypolyA.
 -- numerator can be separable but simplification only possible if denom not separable.
codifyPolyD :: Expr -> (Maybe Code, Maybe Expr)
codifyPolyD expr
    | isNothing div = (Nothing, Just $ Div (decodePoly upper') (decodePoly lower'))
    | otherwise = (div, Nothing)
    where
    lower = if (isNeg expr) then (getNeg $ getLower expr) else (getLower expr)
    upper = getUpper expr -- since we want to keep minus
    upper' = if (isSeparable upper) then (codifyPolyA upper) else (codifyMono upper)
    lower' = if (isSeparable lower) then (codifyPolyA lower) else (codifyMono lower)
    div = divPoly upper' lower'


-- precondition: must not start with neg! Must have gone through distribute
-- function so that neg is inside.
-- postcondition: returns (nothing, chiseled expr) if expr was div and had separable bottom
-- and returns (code, nothing) if succeeded to simplify.
codifyPoly :: Expr -> (Maybe Code, Maybe Expr)
codifyPoly e
    | isSeparable e && (all isMono es) = (Just $ foldl1 addPoly $ map codifyMono es, Nothing)
    | isSeparable e = (Just mulsCode, Just divsExprFromDiv)
    | hasDiv e && (isDiv expDiv) = codifyPolyD expDiv
    | hasDiv e && (isMul expDiv) = (Just $ codifyPolyM expDiv, Nothing)
    | otherwise = (Just $ codifyPolyM expDiv, Nothing)
    where
    (Neg en) = e
    expDiv = chisel e
    es = splitAS expDiv
    (divs, muls) = partition (\e -> isDiv e || identifyNegDiv e) es
    (emPairs, cmPairs) = partition (\(cm,em) -> isJust em) (map codifyPolyD divs)
    mulsCodeFromMul = map codifyMono muls
    mulsCodeFromDiv = catMaybes $ fst $ unzip cmPairs
    mulsCode = foldl1 addPoly (mulsCodeFromMul ++ mulsCodeFromDiv)
    divsExprFromDiv = rebuildAS $ catMaybes $ snd $ unzip emPairs

{-

ee = Num 7 .* x .^ Num 2 .- Num (-3) .* Num 5 .* x .^ Num 8 .+ e7
ee7 = chisel $ distribute $ negExplicit ee
dive = (splitAS (chisel $ distribute $ negExplicit e7)) !! 0


exprs = splitAS $ chisel $ distribute $ negExplicit e7
(divsd, muls) = partition (\e -> isDiv e || identifyNegDiv e) exprs
(emPairs, cmPairs) = partition (\(cm,em) -> isJust em) (map codifyPolyD divsd)
mulsCodeFromMul = map codifyMono muls
mulsCodeFromDiv = catMaybes $ fst $ unzip cmPairs
mulsCode = foldl1 addPoly (mulsCodeFromMul ++ mulsCodeFromDiv)
divsExprFromDiv = rebuildAS $ catMaybes $ snd $ unzip emPairs
-}



decodePoly :: Code -> Expr
decodePoly (Poly ps) = rebuildAS $ filter notZero (map clean polynomials)
    where
    notZero x = (not (x == Num 0))
    ns = map Frac ps
    xs = replicate (length ns) x
    pows = map Num $ [0 .. (length ns - 1)]
    xs' = zipWith Pow xs pows
    nxs = zipWith Mul ns xs'
    polynomials = filter notZero $ map (clean . negExplicit) nxs



-- Note all these functions with poly below assume the ps inside the Poly are added.
addPoly :: Code -> Code -> Code
addPoly (Poly ps) (Poly qs) = Poly (zipWith (+) ps' qs')
    where
    len = max (length ps) (length qs)
    fracZero = makeFraction 0
    (ps', qs') = (elongate len fracZero ps, elongate len fracZero qs)


subPoly :: Code -> Code -> Code
subPoly (Poly ps) (Poly qs) = Poly (zipWith (-) ps' qs')
    where
    len = max (length ps) (length qs)
    fracZero = makeFraction 0
    (ps', qs') = (elongate len fracZero ps, elongate len fracZero qs)


mulPoly :: Code -> Code -> Code
mulPoly (Poly ps) (Poly qs) = foldl1 addPoly products'
    where
    ts = zip ps [0..(length ps - 1)] -- ps can be rate type, but pows must be int type.
    maxPow = maximum $ map (\(Poly ps) -> length ps) products
    products = map (\(n, p) -> mulOnePoly n p qs) ts
    products' = map Poly $ map (\(Poly ps) -> ps ++ replicate (maxPow - length ps) 0) products


-- note n = coeff of poly, p = pow of poly with coeff n, q = pow of multiplied poly (accumulated)
-- (m:ms) = elements of other polynomial (added), acc = accumulated multiplications (is a list of
-- tuples that holds first the new coeff value and second the power of this coeff.
-- note Rate constructor holds RationalNum type which shadows Ratio Int
mulOnePoly :: Fraction -> Int -> [Fraction] -> Code
mulOnePoly (Rate n) p ms = foldl addPoly (Poly [makeFraction 0]) polyCs
    where
    ms' = map (\(Rate m) -> m) ms
    ts = mul' n p 0 ms' []
    ts' = map (\(f,s) -> (Rate f, s)) ts
    maxPow = maximum $ map snd ts
    zzs = replicate (maxPow + 1) 0
    cs = map (\(c,p) -> put p c zzs) ts'
    polyCs = map Poly cs
    mul' _ _ _ [] acc = acc
    mul' 0 _ _ _ acc = [(0, 0)] ++ acc
    mul' n p q (m:ms) acc
        | n * m == 0 = mul' n p (q + 1) ms acc
        | otherwise = mul' n p (q + 1) ms (acc ++ [(n * m, p + q)])

{- TODO were used for testing
pps = [Rate (4 % 5), Rate 2, Rate 1, Rate (-8), Rate (1 % 9)]
qqs = [Rate (5 % 4), Rate 9, Rate 0, Rate 1, Rate 4, Rate (-15 % 8), Rate 20]
-}


-- precondition: takes two polys and returns nothing if deom is separable. But ok if denom is glued.
-- We can tell if denom is separable because if mul then there is only one nonzero element.
-- note because we return from frunction if any numer ind are < denom ind, we don't worry about
-- returning negative pow in divOnePoly
divPoly :: Code -> Code -> Maybe Code
divPoly (Poly ns) (Poly ms)
    | denomHasMoreThanOneTerm || someNumPowsLessThanDenomPows = Nothing
    | otherwise = Just $ foldl1 addPoly quotients
    where
    notZero x = not (x == (Rate 0))
    denomHasMoreThanOneTerm = (length $ filter notZero ms) > 1
    someNumPowsLessThanDenomPows = any (== True) $ map (\pow -> pow < dp) ps
    dp = head $ findIndices notZero ms
    d = ms !! dp
    ns' = filter notZero ns
    ps = findIndices notZero ns -- the expoonent of the variables of the nums (coeffs).
    npPairs = zip ns' ps
    ndpTriples = map (divOnePoly (d, dp)) npPairs
    maxPow = maximum $ map snd ndpTriples
    zs = map makeFraction $ replicate (maxPow + 1) 0
    quotients = map Poly $ map (\(f,p) -> put p f zs) ndpTriples


divOnePoly :: (Fraction, Int) -> (Fraction, Int) -> (Fraction, Int)
divOnePoly (Rate den, dPow) (Rate num, nPow) = (Rate $ (a * b) % (c * d), nPow - dPow)
    where (a, b, c, d) = (numerator num, denominator num, numerator den, denominator den)


--------------------- Dealing with Poly-Func (single func) ------------------------



-- example: if functino is in "cos" family (arccos, arccosH, cos, cosh), then it gets put in certain
-- location: sin, cos,tan, csc, sec, cot
-- precondition: takes either pow expression with function or simple function expr.
findLoc :: Expr -> Int
findLoc f
    | isSinFamily f = 0
    | isCosFamily f = 1
    | isTanFamily f = 2
    | isCscFamily f = 3
    | isSecFamily f = 4
    | isCotFamily f = 5
    | isE f = 0
    | isLn f = 1
    | isLog f = 2


-- precondition: expression must not be separable and must have JUST ONE function.
-- postcondition: returns (poly part glued, function)
-- TODO is it possible to change state (return expr without function) and at the same time to
-- return a value? (Want to return changed function as well as removed function in one go...?)
unjoinPolyFunc :: Expr -> (Expr, Expr)
unjoinPolyFunc expr = (chisel $ pluck expr, head $ getFunc expr)
    where
    getFunc (Num _) = []
    getFunc (Frac _) = []
    getFunc (Var _) = []
    getFunc (Neg e) = getFunc e
    getFunc (F f) = [F f]
    getFunc p@(Pow (F f) _) = [p]
    getFunc (Pow base expo) = fmap (\result -> Pow result expo) (getFunc base)
    getFunc e = getFunc (left e) ++ getFunc (right e)

    pluck (Num n) = Num n
    pluck (Frac f) = Frac f
    pluck (Var x) = Var x
    pluck (Neg e) = Neg $ pluck e
    pluck (F f) = Num 1
    pluck (Add e1 e2) = pluck e1 .+ pluck e2
    pluck (Sub e1 e2) = pluck e1 .- pluck e2
    pluck (Mul e1 e2) = pluck e1 .* pluck e2
    pluck (Div e1 e2) = pluck e1 ./ pluck e2
    pluck (Pow (F f) _) = Num 1 -- would be zero but expr is not separable so use 1 as id to mult.
    pluck (Pow base expo) = (pluck base) .^ expo



{-
-- precondition: gets something like e27 or pfhard which has structure: (top) / (bottom * func ^ 7)
-- Simplifies this reasonably without separating func from poly until necessary.
handlePolyFunc :: Expr -> Expr
handlePolyFunc expr
    | isDiv expr' = handleDiv expr'
    | otherwise = handleMul expr'
    where
    expr' = chisel expr
    handleDiv e = Div upper' lower'
        where
        (lower, upper) = (getLower e, getUpper e)
        lower' = if (hasOnlyOneFunction lower) then (meltPolyFunc lower) else (meltPoly lower)
        upper' = if (hasOnlyOneFunction upper) then (meltPolyFunc upper) else (meltPoly lower)
    handleMul e = Mul (decodePoly $ codifyPoly poly) (func) -- TODO clear up function exprs pfhard
        where
        (poly, func) = unjoinPolyFunc e
-}

-- precondition: takes something like  x^3*8x^4*sin^(8x) (3+x) (many of them that are separable)
-- with the function never being onthe bottom as denom in division.
-- postcondition: returns simplified version of it and rebuildsAS
-- note gets input from distribute function in simplify.
meltPolyFunc :: Expr -> Expr
meltPolyFunc (Num 0) = Num 0
meltPolyFunc expr = rebuildAS es'
    where
    es = splitAS expr
    es' = map decodePolyFunc $ concat $ addCodes $ map codifyPolyFunc es



-- precondition: gets something like x^3*8x^4*sin^(8x) (3+x) with the function never being on
-- hte bottom as denom in division.
-- postcondition: simplified polynomials at front in code and function arg,coef,pow in code.
codifyPolyFunc :: Expr -> Code
codifyPolyFunc expr
    | isTrig f = Trig codes
    | isInvTrig f = InvTrig codes
    | isHyp f = Hyperbolic codes
    | isInvHyp f = InvHyp codes
    | isLogar f = Logarithmic codes
    where
    (poly, func) = unjoinPolyFunc expr
    coef = meltPoly poly
    f = getBase func
    descr = (coef, simplify $ getPow func, simplify $ getArg f)
    zs = replicate 6 (Num 0, Num 0, Num 0)
    zsLog = replicate 3 (Num 0, Num 0, Num 0)
    codes = if (isLogFamily f) then (put (findLoc f) descr zsLog) else (put (findLoc f) descr zs)

(poly, func) = unjoinPolyFunc expr
co = meltPoly poly
f = getBase func
descr = (co, simplify $ getPow func, simplify $ getArg f)
zs = replicate 6 (Num 0, Num 0, Num 0)
zsLog = replicate 3 (Num 0, Num 0, Num 0)
codes = if (isLogFamily f) then (put (findLoc f) descr zsLog) else (put (findLoc f) descr zs)





cs = concat $ addCodes (map codifyPolyFunc (splitAS (prepExpr e4)))
(Trig ts) = head cs
gs = map F [Sin x, Cos x, Tan x, Csc x, Sec x, Cot x]
args = map (\(_,_,x) -> x) ts
pows = map (\(_,p,_) -> p) ts
coefs = map (\(c,_,_) -> c) ts
gs' = zipWith Mul coefs (zipWith Pow (zipWith push args gs) pows)



decodePolyFunc :: Code -> Expr
decodePolyFunc (Trig ts) = polyFuncDecoder 0 ts
decodePolyFunc (InvTrig ts) = polyFuncDecoder 1 ts
decodePolyFunc (Hyperbolic hs) = polyFuncDecoder 2 hs
decodePolyFunc (InvHyp hs) = polyFuncDecoder 3 hs
decodePolyFunc (Logarithmic ls) = polyFuncDecoder 4 ls


-- note the Int is a code: 0 = Trig, 1  = InvTrig, 2 = Hyperbolic, 3 =InvHyp, 4 = Logarithmic.
polyFuncDecoder :: Int -> [Description] -> Expr
polyFuncDecoder n ts = rebuildAS $ map clean fs'
    where
    -- ts' = filter (\(c,_,_) -> (not (c == Num 0)) && (not (c == Frac (Rate 0)))) ts
    args = map (\(_,_,x) -> x) ts
    coefs = {-map negExplicit $ -}map (\(c,_,_) -> c) ts
    pows = map (\(_,p,_) -> p) ts
    fs = findFuncFamily n
    fs' = zipWith Mul coefs (zipWith Pow (zipWith push args fs) pows)

    findFuncFamily 0 = map F [Sin x, Cos x, Tan x, Csc x, Sec x, Cot x]
    findFuncFamily 1 = map F [Arcsin x, Arccos x, Arctan x, Arccsc x, Arcsec x, Arccot x]
    findFuncFamily 2 = map F [Sinh x, Cosh x, Tanh x, Csch x, Sech x, Coth x]
    findFuncFamily 3 = map F [Arcsinh x, Arccosh x, Arctanh x, Arccsch x, Arcsech x, Arccoth x]
    findFuncFamily 4 = map F [E x, Ln x, Log x x]




-- note takes list of [Trig [..], InvTrig [], Trig []...] and gathers all same family
-- functions and adds them up. Gathers them in order: ts, its, hs, ihs, ls.
-- Returns list because we many have same family func left that are not addable.
addCodes :: [Code] -> [[Code]]
addCodes codes = map adder (gatherCodes codes)


-- testing
{-
as = concat $ addCodes (fst (splitAt (length ys `div` 5) ys))
xs = concat $ map unwrapCode $ concat $ addCodes ys'


ys' = (fst (splitAt (length ys `div` 5) ys))
cs' = map unwrapCode ys'
add (c1,p1,x1) (c2,p2,x2) = (c1 .+ c2, p1,x1)
groups = map (map (foldl1 add)) (map gatherArgsPows (transpose cs'))
groups' = transpose $ map (elongate (maximum $ map length groups) (Num 0, Num 0, Num 0)) groups
notAllZero xs = not (all (\x -> x == (Num 0, Num 0, Num 0)) xs)
groups'' = map (map (\(c,p,x) -> (simplify c,p,x))) (filter notAllZero groups')
-}

prep = chisel . distribute . negExplicit
fsc = map codifyPolyFunc (map prep (splitAS (prep e)))
cs' = map unwrapCode fsc
add (c1,p1,x1) (c2,p2,x2) = (c1 .+ c2, p1,x1)
groups = map (map (foldl1 add)) (map gatherArgsPows (transpose cs'))
groups' = transpose $ map (elongate (maximum $ map length groups) (Num 0, Num 0, Num 0)) groups
notAllZero xs = not (all (\x -> x == (Num 0, Num 0, Num 0)) xs)
groups'' = filter notAllZero $ map (map (\(c,p,x) -> (simplify c,p,x))) groups'


adder :: [Code] -> [Code]
adder [] = []
adder cs = map const groups''
    where
    cs' = map unwrapCode cs
    groups = map (map (foldl1 add)) (map gatherArgsPows (transpose cs'))
    groups' = transpose $ map (elongate maxLen zeroes) groups
    groups'' = filter notAllZero $ map (map (\(c,p,x) -> (simplify c,p,x))) groups'
    const = getConstr (getCode (head cs))
    zeroes = (Num 0, Num 0, Num 0)
    maxLen = maximum $ map length groups
    add (c1,p1,x1) (c2,p2,x2) = (c1 .+ c2, p1,x1)
    notAllZero xs = not (all (\x -> x == (Num 0, Num 0, Num 0)) xs)



getConstr :: Int -> ([Description] -> Code)
getConstr 0 = Trig
getConstr 1 = InvTrig
getConstr 2 = Hyperbolic
getConstr 3 = InvHyp
getConstr 4 = Logarithmic



gatherArgsPows :: [Description] -> [[Description]]
gatherArgsPows ds = filter (not . null) $ gather [[]] ds
    where
    gatherOne (c,p,x) ds = partition (\(c',p',x') -> p == p' && x == x') ds

    gather acc [] = acc
    gather acc (d:ds) = gather (acc ++ [concat [[d], like]]) other
        where (like, other) = gatherOne d ds


gatherCodes :: [Code] -> [[Code]]
gatherCodes [] = [[]]
gatherCodes codes = gather' [] [] [] [] [] codes
    where
    gather' ts its hs ihs ls [] = [ts] ++ [its] ++ [hs] ++ [ihs] ++ [ls]
    gather' ts its hs ihs ls (xs@(Trig _) : rest)
        = gather' (ts ++ [xs]) its hs ihs ls rest
    gather' ts its hs ihs ls (xs@(InvTrig _) : rest)
        = gather' ts (its ++ [xs]) hs ihs ls rest
    gather' ts its hs ihs ls (xs@(Hyperbolic _) : rest)
        = gather' ts its (hs ++ [xs]) ihs ls rest
    gather' ts its hs ihs ls (xs@(InvHyp _) : rest)
        = gather' ts its hs (ihs ++ [xs]) ls rest
    gather' ts its hs ihs ls (xs@(Logarithmic _) : rest)
        = gather' ts its hs ihs (ls ++ [xs]) rest




-- brings the neg out to the front
negExplicit :: Expr -> Expr
negExplicit expr
    | expr' == expr = expr
    | otherwise = negExplicit expr'
    where
    expr' = neg expr
    neg e@(Num n) = if n < 0 then (Neg (Num (-1*n))) else e
    neg e@(Frac (Rate n)) = if n < 0 then (Neg (Frac (Rate (-1*n)))) else e
    neg e@(F f) = e
    neg e@(Var x) = e
    neg (Neg (Neg n)) = neg n
    neg (Neg n) = Neg $ neg n

    -- Note tehse are removed from clean() and put here temporarily.
    neg (Add (Neg a) (Neg b)) = Neg $ neg a .+ neg b
    neg (Add a (Neg b)) = neg a .- neg b
    neg (Add (Neg a) b) = Neg $ neg a .- neg b
    neg (Sub (Neg a) (Neg b)) = Neg $ neg a .- neg b
    neg (Sub a (Neg b)) = neg a .+ neg b
    neg (Sub (Neg a) b) = Neg $ neg a .+ neg b
    -- note like above from clean and put here.
    neg (Mul (Neg a) (Neg b)) = neg a .* neg b
    neg (Mul (Neg a) b) = Neg $ neg a .* neg b
    neg (Mul a (Neg b)) = Neg $ neg a .* neg b
    neg (Div (Neg a) (Neg b)) = neg a ./ neg b
    neg (Div (Neg a) b) = Neg $ neg a ./ neg b
    neg (Div a (Neg b)) = Neg $ neg a ./ neg b

    neg e@(Add a b) = Add (neg a) (neg b)
    neg e@(Sub a b) = Sub (neg a) (neg b)
    neg e@(Mul a b) = Mul (neg a) (neg b)
    neg e@(Div a b) = Div (neg a) (neg b)
    neg e@(Pow a b) = Pow (neg a) (neg b)


{-note was used for testing decodepolyfunc
ts = map numify [(-4,1,x), (3,1,x), (1,7,(Num 2) .* x .^ Num 5), (-2,2,x),(1,1,x), (7,7,Num 7 .* x)]
us = ts-}




getCode :: Code -> Int
getCode (Trig _) = 0
getCode (InvTrig _) = 1
getCode (Hyperbolic _) = 2
getCode (InvHyp _) = 3
getCode (Logarithmic _) = 4


unwrapCode :: Code -> [Description]
unwrapCode (Trig ts) = ts
unwrapCode (InvTrig ts) = ts
unwrapCode (Hyperbolic ts) = ts
unwrapCode (InvHyp ts) = ts
unwrapCode (Logarithmic ts) = ts



-- postcondition: if the pows and args are equal then we add the coeffs.
addCodesM :: Code -> Code -> (Code, Code)
addCodesM ts us
    | code == 0 = (Trig ts'', Trig $ prepMaybeCodes us'')
    | code == 1 = (InvTrig ts'', InvTrig $ prepMaybeCodes us'')
    | code == 2 = (Hyperbolic ts'', Hyperbolic $ prepMaybeCodes us'')
    | code == 3 = (InvHyp ts'', InvHyp $ prepMaybeCodes us'')
    | code == 4 = (Logarithmic ts'', Logarithmic $ prepMaybeCodes us'')
    where
    code = getCode ts
    add a@(c1,p1,x1) b@(c2,p2,x2)
        | (x1 == x2 && p1 == p2) = ((c1 .+ c2, p1, x1), Nothing)
        | otherwise = (a, Just b)
    simpMaybe expr@((c,p,x), maybe)
        | isNothing maybe = ((simplify c, p, x), Nothing)
        | otherwise = expr
    (ts', us') = (unwrapCode ts, unwrapCode us)
    simplifiedMaybes = map simpMaybe $ zipWith add ts' us'
    (ts'', us'') = unzip simplifiedMaybes
    prepMaybeCodes ms = ms''
        where
        ms' = map (\m -> if isNothing m then (Just (Num 0, Num 0, Num 0)) else m) ms
        ms'' = catMaybes ms' -- (removing all the justs and leaving args)


-- note no need to simplify to the x, just to c, and p because the x is simplified
-- before being passed here, in the codifyPolyFunc.
-- postcondition: if the args are equal then we mul coeffs and add pows.
mulCodesM :: Code -> Code -> (Code, Code)
mulCodesM ts us
    | code == 0 = (Trig ts'', Trig $ prepMaybeCodes us'')
    | code == 1 = (InvTrig ts'', InvTrig $ prepMaybeCodes us'')
    | code == 2 = (Hyperbolic ts'', Hyperbolic $ prepMaybeCodes us'')
    | code == 3 = (InvHyp ts'', InvHyp $ prepMaybeCodes us'')
    | code == 4 = (Logarithmic ts'', Logarithmic $ prepMaybeCodes us'')
    where
    code = getCode ts
    mul a@(c1,p1,x1) b@(c2,p2,x2)
        | (x1 == x2) = ((c1 .* c2, p1 .+ p2, x1), Nothing)
        | otherwise = (a, Just b)
    simpMaybe expr@((c,p,x), maybe)
        | isNothing maybe = ((simplify c, simplify p, x), Nothing)
        | otherwise = expr
    (ts', us') = (unwrapCode ts, unwrapCode us)
    simplifiedMaybes = map simpMaybe $ zipWith mul ts' us'
    (ts'', us'') = unzip simplifiedMaybes
    prepMaybeCodes ms = ms''
        where
        ms' = map (\m -> if isNothing m then (Just (Num 0, Num 0, Num 0)) else m) ms
        ms'' = catMaybes ms' -- (removing all the justs and leaving args)



-- postcondition: if the args are equal then we div coeffs and subtract pows.
-- note help why doesn't this work:
    -- | (x1 == x2) && (numOrFrac c1 c2) = ((Frac $ Rate zn, p1 .- p2, x1), Nothing)
divCodesM :: Code -> Code -> (Code, Code)
divCodesM ts us
    | code == 0 = (Trig ts'', Trig $ prepMaybeCodes us'')
    | code == 1 = (InvTrig ts'', InvTrig $ prepMaybeCodes us'')
    | code == 2 = (Hyperbolic ts'', Hyperbolic $ prepMaybeCodes us'')
    | code == 3 = (InvHyp ts'', InvHyp $ prepMaybeCodes us'')
    | code == 4 = (Logarithmic ts'', Logarithmic $ prepMaybeCodes us'')
    where
    code = getCode ts
    div a@(c1,p1,x1) b@(c2,p2,x2)
        | (x1 == x2) = ((c1 ./ c2, p1 .- p2, x1), Nothing)
        | otherwise = (a, Just b)
    simpMaybe expr@((c,p,x), maybe)
        | isNothing maybe = ((simplify c, simplify p, x), Nothing)
        | otherwise = expr
    (ts', us') = (unwrapCode ts, unwrapCode us)
    simplifiedMaybes = map simpMaybe $ zipWith div ts' us'
    (ts'', us'') = unzip simplifiedMaybes
    prepMaybeCodes ms = ms''
        where
        ms' = map (\m -> if isNothing m then (Just (Num 0, Num 0, Num 0)) else m) ms
        ms'' = catMaybes ms' -- (removing all the justs and leaving args)




latch zs = map (\((c,e,u), f) -> c .* (push u f) .^ e) (map (\((tup, f)) -> (numify tup, f)) zs)
numify (c,e,u) = (Num c, Num e, u)








-- TODO make polynomials and trigs and other functions be raised to powers of functions
-- not just constants (make the middle element in tuple to be an expression)
-- While you're at it, make the first coeff an expression so that we can do xsin(x) + 2xsin(x)
-- TODO return error if one of the tuples has patterns (1, 0, ..) because that yields a constant.
-- NOTE rules:
-- Poly: [1, 0, 2, 7, -4] represents 1 + x^2 + 7x^3 - 4x^4
-- Trig: [(0,1), (1,1), (4,1), (1,1), (2,4), (3, 5)]
--      represents (cos x + 4tan x + csc x + 2sec^4 x  + 3cot^5 x
--      where x = any expression and note (0, 1) = 0 always and (1,0) gets error
-- help TODO fix so that it handles (expr, expr, expr) and not (int, int, expr)
{-
groupToExpr :: Group -> [Expr]
groupToExpr (Poly ps) = reverse $ map simplify $ zipWith (.*) ps' (zipWith (.^) xs es )
    where xs = replicate (length ps) X
          es = map Num [0 .. (length ps - 1)]
          ps' = map Num ps
-- TODO inside function is holder -- find way to deal with that.
groupToExpr (Trig ts) = map simplify $ latch (zip ts fs)
    where fs = map F [Sin X, Cos X, Tan X, Csc X, Sec X, Cot X]
groupToExpr (InvTrig ts) = map simplify $ latch (zip ts fs)
    where fs = map F [Arcsin X, Arccos X, Arctan X, Arccsc X, Arcsec X, Arccot X]
groupToExpr (Hyperbolic hs) = map simplify $ latch (zip hs fs)
    where fs = map F [Sinh X, Cosh X, Tanh X, Csch X, Sech X, Coth X]
groupToExpr (InvHyp hs) = map simplify $ latch  (zip hs fs)
    where fs = map F [Arcsinh X, Arccosh X, Arctanh X, Arccsch X, Arcsech X, Arccoth X]
groupToExpr (Logarithmic ls) = map simplify $ latch (zip ls fs)
    where fs = map F [E X, Ln X, Log X X]  -- TODO fix so we can have different args for log base and arg.
-}








isAdd :: Expr -> Bool
isAdd (Add _ _) = True
isAdd _ = False

isSub :: Expr -> Bool
isSub (Sub _ _) = True
isSub _ = False

isMul :: Expr -> Bool
isMul (Mul _ _) = True
isMul _ = False

isDiv :: Expr -> Bool
isDiv (Div _ _) = True
isDiv _ = False

getUpper :: Expr -> Expr
getUpper (Div numer _) = numer
getUpper (Neg (Div numer _)) = Neg numer
getUpper _ = error "no div expr in getUpper"

getLower :: Expr -> Expr
getLower (Div _ denom) = denom
getLower (Neg (Div _ denom)) = Neg $ denom
getLower _ = error "no div expr in getLower"

isPow :: Expr -> Bool
isPow (Pow _ _) = True
isPow _ = False

getPow :: Expr -> Expr
getPow (Pow _ expo) = expo
getPow (Neg (Pow _ expo)) = expo
getPow expr = Num 1 -- if not an actual power, then expo is 1

getBase :: Expr -> Expr
getBase (Pow base _) = base
getBase (Neg (Pow base _)) = base
getBase expr = expr

isNum :: Expr -> Bool
isNum (Num _) = True
isNum (Neg (Num _)) = True
isNum _ = False

getNum :: Expr -> Int
getNum (Num n) = n
getNum (Neg (Num n)) = -n

isFrac :: Expr -> Bool
isFrac (Frac _) = True
isFrac (Neg (Frac _)) = True
isFrac _ = False

getFrac :: Expr -> Fraction
getFrac (Frac f) = f
getFrac (Neg (Frac f)) = -f

getNumOrFrac :: Expr -> Fraction
getNumOrFrac expr
    | isNum expr  = Rate $ (getNum expr) % 1
    | isFrac expr = getFrac expr

isNegNumOrFrac :: Expr -> Bool
isNegNumOrFrac (Neg (Num _)) = True
isNegNumOrFrac (Neg (Frac _)) = True
isNegNumOrFrac _ = False

isNumOrFracNeg :: Expr -> Bool
isNumOrFracNeg (Num n) = n < 0
isNumOrFracNeg (Frac f) = f < 0
isNumOrFracNeg _ = False

isVar :: Expr -> Bool
isVar (Var _) = True
isVar _ = False

isNeg :: Expr -> Bool
isNeg (Neg e) = True
isNeg _ = False

getNeg :: Expr -> Expr
getNeg (Neg n) = n
getNeg _ = error "incorrect argument"


--- Trigonometric ---
isSin :: Expr -> Bool
isSin (F (Sin _)) = True
isSin _ = False

isCos :: Expr -> Bool
isCos (F (Cos _)) = True
isCos _ = False

isTan :: Expr -> Bool
isTan (F (Tan _)) = True
isTan _ = False

isCsc :: Expr -> Bool
isCsc (F (Csc _)) = True
isCsc _ = False

isSec :: Expr -> Bool
isSec (F (Sec _)) = True
isSec _ = False

isCot :: Expr -> Bool
isCot (F (Cot _)) = True
isCot _ = False

isArcsin :: Expr -> Bool
isArcsin (F (Arcsin _)) = True
isArcsin _ = False

isArccos :: Expr -> Bool
isArccos (F (Arccos _)) = True
isArccos _ = False

isArctan :: Expr -> Bool
isArctan (F (Arctan _)) = True
isArctan _ = False

isArccsc :: Expr -> Bool
isArccsc (F (Arccsc _)) = True
isArccsc _ = False

isArcsec :: Expr -> Bool
isArcsec (F (Arcsec _)) = True
isArcsec _ = False

isArccot :: Expr -> Bool
isArccot (F (Arccot _)) = True
isArccot _ = False

--- Hyperbolic ---

isSinh :: Expr -> Bool
isSinh (F (Sinh _)) = True
isSinh _ = False

isCosh :: Expr -> Bool
isCosh (F (Cosh _)) = True
isCosh _ = False

isTanh :: Expr -> Bool
isTanh (F (Tanh _)) = True
isTanh _ = False

isCsch :: Expr -> Bool
isCsch (F (Csch _)) = True
isCsch _ = False

isSech :: Expr -> Bool
isSech (F (Sech _)) = True
isSech _ = False

isCoth :: Expr -> Bool
isCoth (F (Coth _)) = True
isCoth _ = False

isArcsinh :: Expr -> Bool
isArcsinh (F (Arcsinh _)) = True
isArcsinh _ = False

isArccosh :: Expr -> Bool
isArccosh (F (Arccosh _)) = True
isArccosh _ = False

isArctanh :: Expr -> Bool
isArctanh (F (Arctanh _)) = True
isArctanh _ = False

isArccsch :: Expr -> Bool
isArccsch (F (Arccsch _)) = True
isArccsch _ = False

isArcsech :: Expr -> Bool
isArcsech (F (Arcsech _)) = True
isArcsech _ = False

isArccoth :: Expr -> Bool
isArccoth (F (Arccoth _)) = True
isArccoth _ = False

--- Logarithmic ---

isLn :: Expr -> Bool
isLn (F (Ln _)) = True
isLn _ = False

isLog :: Expr -> Bool
isLog (F (Log _ _)) = True
isLog _ = False

isE :: Expr -> Bool
isE (F (E _)) = True
isE _ = False

--- Function families ---
isSinFamily :: Expr -> Bool
isSinFamily e = isSin e || isSinh e || isArcsin e || isArcsinh e

isCosFamily :: Expr -> Bool
isCosFamily e = isCos e || isCosh e || isArccos e || isArccosh e

isTanFamily :: Expr -> Bool
isTanFamily e = isTan e || isTanh e || isArctan e || isArctanh e

isCscFamily :: Expr -> Bool
isCscFamily e = isCsc e || isCsch e || isArccsc e || isArccsch e

isSecFamily :: Expr -> Bool
isSecFamily e = isSec e || isSech e || isArcsec e || isArcsech e

isCotFamily :: Expr -> Bool
isCotFamily e = isCot e || isCoth e || isArccot e || isArccoth e

isLogFamily :: Expr -> Bool
isLogFamily e = isE e || isLn e || isLog e

---------------------------------
-- does not refer to data Op, just data Expr (Add, sub, mul..)
isOp :: Expr -> Bool
isOp e = isAdd e || isSub e || isMul e || isDiv e || isPow e


isVarNumFunc :: Expr -> Bool
isVarNumFunc (Var _) = True
isVarNumFunc (Num _) = True
isVarNumFunc (F f) = True
isVarNumFunc _ = False

hasAdd :: Expr -> Bool
hasAdd (Var _) = False
hasAdd (Add _ _) = True
hasAdd (Num _) = False
hasAdd (Frac _) = False
hasAdd (Neg e) = hasAdd e
hasAdd (F f) = False
hasAdd (Sub e1 e2) = hasAdd e1 || hasAdd e2
hasAdd (Mul e1 e2) = hasAdd e1 || hasAdd e2
hasAdd (Div e1 e2) = hasAdd e1 || hasAdd e2
hasAdd (Pow e1 e2) = hasAdd e1 || hasAdd e2

hasSub :: Expr -> Bool
hasSub (Var _) = False
hasSub (Sub _ _) = True
hasSub (Num _) = False
hasSub (Frac _) = False
hasSub (Neg e) = hasSub e
hasSub (F f) = False
hasSub (Add e1 e2) = hasSub e1 || hasSub e2
hasSub (Mul e1 e2) = hasSub e1 || hasSub e2
hasSub (Div e1 e2) = hasSub e1 || hasSub e2
hasSub (Pow e1 e2) = hasSub e1 || hasSub e2

hasMul :: Expr -> Bool
hasMul (Var _) = False
hasMul (Mul _ _) = True
hasMul (Num _) = False
hasMul (Frac _) = False
hasMul (Neg e) = hasMul e
hasMul (F f) = False
hasMul (Add e1 e2) = hasMul e1 || hasMul e2
hasMul (Sub e1 e2) = hasMul e1 || hasMul e2
hasMul (Div e1 e2) = hasMul e1 || hasMul e2
hasMul (Pow e1 e2) = hasMul e1 || hasMul e2

hasDiv :: Expr -> Bool
hasDiv (Var _) = False
hasDiv (Div _ _) = True
hasDiv (Num _) = False
hasDiv (Frac _) = False
hasDiv (Neg e) = hasDiv e
hasDiv (F f) = False
hasDiv (Add e1 e2) = hasDiv e1 || hasDiv e2
hasDiv (Sub e1 e2) = hasDiv e1 || hasDiv e2
hasDiv (Mul e1 e2) = hasDiv e1 || hasDiv e2
hasDiv (Pow e1 e2) = hasDiv e1 || hasDiv e2

hasFunction :: Expr -> Bool
hasFunction (Var _) = False
hasFunction (F _) = True
hasFunction (Num _) = False
hasFunction (Frac _) = False
hasFunction (Neg e) = hasFunction e
hasFunction (Add e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Sub e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Mul e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Div e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Pow e1 e2) = hasFunction e1 || hasFunction e2


hasNeg :: Expr -> Bool
hasNeg (Var _) = False
hasNeg (F _) = False
hasNeg (Num n) = n < 0
hasNeg (Frac f) = f < 0
hasNeg (Neg _) = True
hasNeg (Add e1 e2) = hasNeg e1 || hasNeg e2
hasNeg (Sub e1 e2) = hasNeg e1 || hasFunction e2
hasNeg (Mul e1 e2) = hasNeg e1 || hasNeg e2
hasNeg (Div e1 e2) = hasNeg e1 || hasNeg e2
hasNeg (Pow e1 e2) = hasNeg e1 || hasNeg e2


hasNegPow :: Expr -> Bool
hasNegPow (Var _) = False
hasNegPow (F _) = False
hasNegPow (Num _) = False
hasNegPow (Frac _) = False
hasNegPow (Pow _ (Neg (Num n))) = n > 0
hasNegPow (Pow _ (Num n)) = n > 0
hasNegPow (Pow _ (Neg (Frac f))) = f > 0
hasNegPow (Pow _ (Frac f)) = f > 0
hasNegPow (Neg e) = hasNegPow e
hasNegPow (Add e1 e2) = hasNegPow e1 || hasNegPow e2
hasNegPow (Sub e1 e2) = hasNegPow e1 || hasNegPow e2
hasNegPow (Mul e1 e2) = hasNegPow e1 || hasNegPow e2
hasNegPow (Div e1 e2) = hasNegPow e1 || hasNegPow e2
hasNegPow (Pow e1 e2) = hasNegPow e1 || hasNegPow e2



-- counts total number of separate terms, where powers are counted as 1 term.
numTerms :: Expr -> Int
numTerms expr = length $ divid
    where
    added = split AddOp expr
    subbed = concatMap (split SubOp) added
    multip = concatMap (split MulOp) subbed
    divid = concatMap (split DivOp) multip


left :: Expr -> Expr
left (Var x) = Var x
left (Num n) = Num n
left (Frac f) = Frac f
left (F f) = F f
left (Neg e) = Neg (left e)
left (Add e1 e2) = e1
left (Sub e1 e2) = e1
left (Mul e1 e2) = e1
left (Div e1 e2) = e1
left (Pow e1 e2) = e1


right :: Expr -> Expr
right (Var x) = Var x
right (Num n) = Num n
right (Frac f) = Frac f
right (F f) = F f
right (Neg e) = Neg (right e)
right (Add e1 e2) = e2
right (Sub e1 e2) = e2
right (Mul e1 e2) = e2
right (Div e1 e2) = e2
right (Pow e1 e2) = e2



notZero :: Expr -> Bool
notZero (Num 0) = False
notZero _ = True


-- rebuilds with add sub signs.
rebuildAS :: [Expr] -> Expr
rebuildAS es = clean $ foldl f (Num 0) es
    where
    f acc x
        | (not (acc == (Num 0))) && isNeg x = (Sub acc (getNeg x))
        | otherwise = Add acc x


rebuild :: Op -> [Expr] -> Expr
rebuild AddOp es = clean $ foldl (\acc x -> Add acc x) (Num 0) es
rebuild SubOp es = clean $ foldl (\acc x -> Sub acc x) (Num 0) es
rebuild MulOp es = clean $ foldl (\acc x -> Mul acc x) (Num 0) es
rebuild DivOp es = clean $ foldl (\acc x -> Div acc x) (Num 0) es
rebuild PowOp es = clean $ foldl (\acc x -> Pow acc x) (Num 0) es



-- START HERE tomorrow 10/14: was working on codifying but first update sweep to save mono
-- IDEA IMPORTANT: make sweep so that it goes into (Add e1 e2) = Add (sweep e1) (sweep e2)
-- and then deal with gathering up constants for glued expressions.

-- need to use sweep constants (make sweep so that
-- note says if expression is a single polynomial term like 5x (monomial)
isMono :: Expr -> Bool
isMono (Neg e) = isMono e
isMono e
    | hasFunction e || isSeparable e = False
    | otherwise = foldl f True s'
    where
    -- e' = simplifyComplete e
    s' = map clean $ split MulOp e -- was e' , changed since got sent to infinite loop.
    f = (\acc x -> acc && (isNum x || isFrac x || isVar x || isPolyPow x))
    isPolyPow (Pow (Var _) (Neg (Num n))) = True
    isPolyPow (Pow (Var _) (Num n)) = True
    isPolyPow (Pow (Var _) (Neg (Frac f))) = True
    isPolyPow (Pow (Var _) (Frac f)) = True
    isPolyPow _ = False


splitAll :: Expr -> [Expr]
splitAll expr = (concatMap (split DivOp) divs) ++ muls
    where (divs, muls) = partition (\e -> isDiv e || identifyNegDiv e) (splitAS expr)

-- note says if expression contains no functions and is just a polynomial term like 7x^2 / 6x or just 5x
-- needs to get input from chisel where either fully div or fully mul.
isPoly :: Expr -> Bool
isPoly e = ((not $ hasFunction e) || (all isMono (splitAll e))) && (not $ isExponential e)


-- returns true if exponential function (fixed base raised to variable power: a^x)
isExponential :: Expr -> Bool
isExponential (Neg e) = isExponential e
isExponential (Pow base expo) = isConstant base && isVariable expo
isExponential _ = False

isConstant :: Expr -> Bool
isConstant (Neg e) = isConstant e
isConstant (Frac f) = True
isConstant (Num n) = True
isConstant _ = False

isVariable :: Expr -> Bool
isVariable = not . isConstant

{-isPoly :: Expr -> Bool
isPoly expr
    | hasFunction expr = False
    | isMono expr = True
    | hasDiv expr = isPoly (split DivOp expr)
    | any hasDiv as = all isPoly (map isPoly as)
    | length as == 1 = False
    | otherwise = all isMono as
    where
    as = splitAS expr
    ds = split DivOp expr-}
{-isPoly (Var _) = False
isPoly (Num n) = True
isPoly (F f) = False
isPoly (Neg e) = isPoly e
isPoly (Add e1 e2) = isPoly e1 && isPoly e2
isPoly (Sub e1 e2) = isPoly e1 && isPoly e2
isPoly (Mul e1 e2) = isPoly e1 && isPoly e2
isPoly (Div e1 e2) = isPoly e1 && isPoly e2
isPoly (Pow e1 e2) = isPoly e1 && isPoly e2-}






-- note this is passed only glued expressions!
-- example 3x^7x^2sin(4x) = True
-- example (sin (sin x)) = True
-- example (sinxtanx) = False
hasOnlyOneFunction :: Expr -> Bool
hasOnlyOneFunction expr = (countFunc 0 expr) == 1

hasManyFunctions :: Expr -> Bool
hasManyFunctions expr = (countFunc 0 expr) > 1

-- note doesn't matter if there is func in expo of a power because even if there is we can
-- put it in description as a simple expo (expos have have any type of expr)
countFunc :: Int -> Expr -> Int
countFunc c (Num n) = c
countFunc c (Var x) = c
countFunc c (Frac f) = c
countFunc c (F f) = c + 1
countFunc c (Neg e) = countFunc c e
countFunc c (Add e1 e2) = countFunc c e1 + countFunc c e2
countFunc c (Sub e1 e2) = countFunc c e1 + countFunc c e2
countFunc c (Mul e1 e2) = countFunc c e1 + countFunc c e2
countFunc c (Div e1 e2) = countFunc c e1 + countFunc c e2
countFunc c (Pow e1 e2) = countFunc c e1 -- + oneFunc count e2

-- note do not count isItem in the exponent of a power because there it doesn't matter as we
-- use the expo in descrip.
-- HELP todo not working ebcause it counts Ops way too many times.
{-count :: (Expr -> Bool) -> Expr -> Int
count f expr = counter 0 f expr
    where
    newC c = if (f x) then (c + 1) else c
    counter c f x@(Num _) = newC c
    counter c f x@(Frac _) = newC c
    counter c f x@(Var _) = newC c
    counter c f x@(F _) = newC c
    counter c f x@(Neg e) = counter (newC c) f e
    counter c f x@(Add e1 e2) = counter (newC c) f e1 + counter (newC c) f e2
    counter c f x@(Sub e1 e2) = counter (newC c) f e1 + counter (newC c) f e2
    counter c f x@(Mul e1 e2) = counter (newC c) f e1 + counter (newC c) f e2
    counter c f x@(Div e1 e2) = counter (newC c) f e1 + counter (newC c) f e2
    counter c f x@(Pow e1 e2) = counter (newC c) f e1 -- + count c' isItem e2-}


{-
count c isItem n@(Num _) = c'
    where c' = if (isItem n) then (c + 1) else c
count c isItem f@(Frac _) = c'
    where c' = if (isItem f) then (c + 1) else c
count c isItem v@(Var _) = c'
    where c' = if (isItem v) then (c + 1) else c
count c isItem f@(F _) = c'
    where c' = if (isItem f) then (c + 1) else c
count c isItem n@(Neg e) = count c' isItem e
    where c' = if (isItem n) then (c + 1) else c

count c isItem a@(Add e1 e2) = count c' isItem e1 + count c' isItem e2
    where c' = if (isItem a) then (c + 1) else c
count c isItem s@(Sub e1 e2) = count c' isItem e1 + count c' isItem e2
    where c' = if (isItem s) then (c + 1) else c
count c isItem m@(Mul e1 e2) = count c' isItem e1 + count c' isItem e2
    where c' = if (isItem m) then (c + 1) else c
count c isItem d@(Div e1 e2) = count c' isItem e1 + count c' isItem e2
    where c' = if (isItem d) then (c + 1) else c
count c isItem p@(Pow e1 e2) = count c' isItem e1 -- + count c' isItem e2
    where c' = if (isItem p) then (c + 1) else c
-}



isSeparable :: Expr -> Bool
isSeparable expr = (length $ splitAS expr) > 1



-- chisel (Mul e (F f)) = chisel e .* (F $ fmap chisel f) -- TODO other way to handle this? pluckfunc?
-- ---> NOTE help check again but seems to work fine now (above)
-- postcondition: converts negative pow to positive by changing to div or mul.
chisel :: Expr -> Expr
chisel e
    -- | (length $ splitAS expr) > 1 = error "expr must not have exterior added/subtracted terms"
    | expr' == expr = clean $ makeDivExplicit expr
    | otherwise = chisel expr'
    where
    expr = clean e
    expr' = if (hasDiv expr) then (chiseler (makeDivExplicit expr)) else (chiseler expr)
    chiseler (Num n) = Num n
    chiseler (Var x) = Var x
    chiseler (Frac f) = Frac f
    chiseler (Neg e) = Neg $ chiseler e
    chiseler (F f) = F f  -- TODO functor here to map inside and chisel the function args.

    --- note the (4x + 8x^(-22)) simplification cases (with neg been pushed to outside
    -- for non-nums and pushed inside for nums)
    chiseler (Add a (Pow b (Num p)))
        | p < 0 = Div (chiseler a .* chiseler b .^ Num (-1*p) .+ Num 1) (chiseler b .^ Num (-1*p))
        | otherwise = chiseler a .+ (chiseler b .^ (Num p))
    chiseler (Add a (Mul (Num n) (Pow b (Num p))))
        | p < 0 = Div (chiseler a .* (chiseler b .^ Num (-1*p)) .+ Num n) (chiseler b .^ Num (-1*p))
        | otherwise = chiseler a .+ (Num n .* (chiseler b .^ Num p))
    {-chiseler (Div (Add a (Mul (Num n) (Pow b (Num p)))) e2)
        | p < 0
            = (Div (chiseler a .* (chiseler b .^ Num (-1*p)) .+ Num n)
            (chiseler b .^ Num (-1*p)))  ./  (chiseler e2)
        | otherwise = chiseler a .+ (Num n .* (chiseler b .^ Num p))
-}
    chiseler (Add (Pow b (Num p)) a)
        | p < 0 = Div (chiseler a .* chiseler b .^ Num (-1*p) .+ Num 1) (chiseler b .^ Num (-1*p))
        | otherwise = (chiseler b .^ Num p) .+ chiseler a
    chiseler (Add (Mul (Num n) (Pow b (Num p))) a)
        | p < 0 = Div (chiseler a .* (chiseler b .^ Num (-1*p)) .+ Num n) (chiseler b .^ Num (-1*p))
        | otherwise = (Num n .* (chiseler b .^ Num p)) .+ chiseler a

    chiseler (Sub a (Pow b (Num p)))
        | p < 0 = Div (chiseler a .* chiseler b .^ Num (-1*p) .- Num 1) (chiseler b .^ Num (-1*p))
        | otherwise = chiseler a .- (chiseler b .^ (Num p))
    chiseler (Sub a (Mul (Num n) (Pow b (Num p))))
        | p < 0 = Div (chiseler a .* (chiseler b .^ Num (-1*p)) .- Num n) (chiseler b .^ Num (-1*p))
        | otherwise = chiseler a .- (Num n .* (chiseler b .^ Num p))

    chiseler (Sub (Pow b (Num p)) a)
        | p < 0 = Div (chiseler a .* chiseler b .^ Num (-1*p) .- Num 1) (chiseler b .^ Num (-1*p))
        | otherwise = (chiseler b .^ Num p) .- chiseler a
    chiseler (Sub (Mul (Num n) (Pow b (Num p))) a)
        | p < 0 = Div (chiseler a .* (chiseler b .^ Num (-1*p)) .- Num n) (chiseler b .^ Num (-1*p))
        | otherwise = (Num n .* (chiseler b .^ Num p)) .- chiseler a


    --- note: the ((n/m)/p) simplification cases
    chiseler (Div (Div a b) (Div c d)) = Div (chiseler a .* chiseler d) (chiseler b .* chiseler c)
    chiseler (Div (Div a b) other) = Div (chiseler a) (chiseler b .* chiseler other)
    chiseler (Div other (Div c d)) = Div (chiseler other .* chiseler d) (chiseler c)

    --- note div power simplify (x+1)^3/(x+1) = (x+1)^2
    chiseler (Div p1@(Pow a (Num n)) p2@(Pow b (Num m)))
        | a == b && (not $ isMono p1) && (not $ isMono p2) = Pow (chiseler a) (Num (n-m))
        | otherwise = Div (chiseler p1) (chiseler p2)

    chiseler (Div p1@(Pow a (Num n)) b)
        | a == b && (not $ isMono p1) && (not $ isMono b) = Pow (chiseler a) (Num (n-1))
        | otherwise = Div (chiseler p1) (chiseler b)
    {-chiseler (Div (Add a b) c)
        | isDiv a' && isDiv b'
            = Div (Add (getUpper a') (getUpper b')) (getLower a' .* getLower b' .* c')
        | isDiv a' = Div (Add (getUpper a') b') (getLower a' .* c')
        | isDiv b' = Div (Add a' (getUpper b')) (getLower b' .* c')
        | otherwise = (a' .+ b') ./ c'
        where
        a' = chiseler a
        b' = chiseler b
        c' = chiseler c
    chiseler (Div (Sub a b) c)
        | isDiv a' && isDiv b'
            = Div (Sub (getUpper a') (getUpper b')) (getLower a' .* getLower b' .* c')
        | isDiv a' = Div (Sub (getUpper a') b') (getLower a' .* c')
        | isDiv b' = Div (Sub a' (getUpper b')) (getLower b' .* c')
        | otherwise = (a' .- b') ./ c'
        where
        a' = chiseler a
        b' = chiseler b
        c' = chiseler c -}
    ------ note the pow cases.
    -- note keeping these despite making div explicit  because result is (7 * 1) / pow
    -- instead of (7 / pow) and we get the latter if we use the below  (for mm3')
    chiseler (Mul a (Pow base (Neg (Num n))))
        | n >= 0 = Div (chiseler a) (Pow (chiseler base) (Num n))
        | otherwise = Mul (chiseler a) (Pow (chiseler base) (Num (-1*n)))
    chiseler ((Mul a (Pow base (Num n))))
        | n < 0 = Div (chiseler a) (Pow (chiseler base) (Num (-1*n)))
        | otherwise = (chiseler a) .* (chiseler base) .^ Num n
    chiseler (Div a (Pow base (Neg (Num n))))
        | n >= 0 = Mul (chiseler a) (Pow (chiseler base) (Num n))
        | otherwise = Div (chiseler a) (Pow (chiseler base) (Num (-1*n)))
    chiseler (Div a (Pow base (Num n)))
        | n < 0 = (chiseler a) .* (chiseler base) .^ Num (-1*n)
        | otherwise = Div (chiseler a) (Pow (chiseler base) (Num n))

    chiseler (Pow base (Neg (Num n)))
        | n >= 0 = Num 1 ./ ((chiseler base) .^ Num n)
        | otherwise = (chiseler base) .^ Num (-1*n)
    chiseler (Pow base (Num n))
        | n < 0 = Num 1 ./ ((chiseler base) .^ (Num (-1*n)))
        | otherwise = (chiseler base) .^ (Num n)

    -- note the mul-covered cases (above must is of form (Mul (case here) e)
    chiseler (Mul (Mul a (Pow base (Neg (Num n)))) side)
        | n >= 0 = Div (chiseler a .* chiseler side) (Pow (chiseler base) (Num n))
        | otherwise = (chiseler a) .* (chiseler base) .^ (Num (-1*n)) .* (chiseler side)
    chiseler (Mul (Mul a (Pow base (Num n))) side)
        | n < 0 = Div (chiseler a .* chiseler side) (Pow (chiseler base) (Num (-1*n)))
        | otherwise = (chiseler a) .* (chiseler base) .^ Num n .* (chiseler side)
    chiseler (Mul (Div a (Pow base (Neg (Num n)))) side)
        | n >= 0 = Mul (chiseler a) (Pow (chiseler base) (Num n)) .* (chiseler side)
        | otherwise = Div (chiseler a .* chiseler side) (Pow (chiseler base) (Num (-1*n)))
    chiseler (Mul (Div a (Pow base (Num n))) side)
        | n < 0 = (chiseler a) .* (chiseler base) .^ Num (-1*n) .* (chiseler side)
        | otherwise = Div (chiseler a .* chiseler side) (Pow (chiseler base) (Num n))

    chiseler (Mul (Pow base (Neg (Num n))) side)
        | n >= 0 = (chiseler side) ./ ((chiseler base) .^ Num n)
        | otherwise = (chiseler base) .^ Num (-1*n) .* (chiseler side)
    chiseler (Mul (Pow base (Num n)) side)
        | n < 0 = (chiseler side) ./ ((chiseler base) .^ (Num (-1*n)))
        | otherwise = (chiseler base) .^ (Num n) .* (chiseler side)


    chiseler (Add e1 e2) = Add (chiseler e1) (chiseler e2)
    chiseler (Sub e1 e2) = Sub (chiseler e1) (chiseler e2)
    chiseler (Mul e1 e2) = Mul (chiseler e1) (chiseler e2)
    chiseler (Div e1 e2) = Div (chiseler e1) (chiseler e2)
    chiseler (Pow e1 e2) = Pow (chiseler e1) (chiseler e2)



-- note puts division expressions on the outside. so (Mul some thing div inside) = > DIv (mul some)
-- precondition: must only deal with glued expressions (div and mul and pow)
-- example it we have Mul ((s) / (t), e) = then returns (Div (s * e, t))
-- example if we have (Mul (s/t, s/t)) then returns the  Div (s*t, s*t)
-- TODO note do we want to simplify here and so use mul-mul cases? if not remvove
makeDivExplicit :: Expr -> Expr
makeDivExplicit expr
    | expr' == expr = expr
    | otherwise = makeDivExplicit expr'
    where
    expr' = explicit expr
    explicit (Num n) = Num n
    explicit (Frac f) = Frac f
    explicit (Var x) = Var x
    explicit (F f) = F f
    explicit (Neg e) = Neg $ explicit e
    explicit (Mul (Div a b) (Div c d)) = Div (a .* b) (c .* d)
    explicit (Mul (Div a b) other) = Div (a .* other) b
    explicit (Mul other (Div c d)) = Div (other .* c) d
    explicit (Mul (Mul a (Div x y)) other) = Div (a .* x .* other) y
    explicit (Mul e1 e2) = Mul (explicit e1) (explicit e2)
    --- TODO note is ok? Took these from here and put them in chisel since these are already
    -- div explicit but just need to be chiseled into simpler form.
    {-explicit (Div (Div a b) (Div c d)) = Div (a .* d) (b .* c)
    explicit (Div (Div a b) other) = Div a (b .* other)
    explicit (Div other (Div c d)) = Div (other .* d) c-}
    explicit (Div e1 e2) = Div (explicit e1) (explicit e2)
    explicit (Pow base expo) = Pow (explicit base) (explicit expo)
    explicit (Add e1 e2) = Add (explicit e1) (explicit e2)
    explicit (Sub e1 e2) = Sub (explicit e1) (explicit e2)

-- TODO  note do we want to simplify here to keep the div-div cases? if not remvove
makeMulExplicit :: Expr -> Expr
makeMulExplicit expr
    | expr' == expr = expr
    | otherwise = makeMulExplicit expr'
    where
    expr' = explicit expr
    explicit (Num n) = Num n
    explicit (Frac f) = Frac f
    explicit (Var x) = Var x
    explicit (F f) = F f
    explicit (Neg e) = Neg $ explicit e
    {-explicit (Mul (Div a b) (Div c d)) = Mul (a ./ b) (d ./ c)
    explicit (Mul (Div a b) other) = Mul (a ./ b) (Num 1 ./ other)
    explicit (Mul other (Div c d)) = Mul (other ./ c) d-}
    explicit (Mul e1 e2) = Mul (explicit e1) (explicit e2)
    explicit (Div (Mul a b) (Mul c d)) = Mul (a ./ c) (b ./ d)
    explicit (Div (Mul a b) other) = Mul (a .* b) (Num 1 ./ other)
    explicit (Div other (Mul c d)) = Mul (other ./ c) (Num 1 ./ d)
    explicit (Div (Div a b) (Div c d)) = Mul (a ./ b) (d ./ c)
    explicit (Div (Div a b) other) = Mul (a ./ b) (Num 1 ./ other)
    explicit (Div other (Div c d)) = Mul (other ./ c) d
    explicit (Div e1 e2) = Div (explicit e1) (explicit e2)
    explicit (Pow base expo) = Pow (explicit base) (explicit expo)
    explicit (Add e1 e2) = Add (explicit e1) (explicit e2)
    explicit (Sub e1 e2) = Sub (explicit e1) (explicit e2)

-- to test with mulexplicit
m1 = (Num 4 ./ Num 3) ./ Num 9 -- should be (4/3) * (1/9)

-- to test with divexplicit
d1 = (Num 1 ./ Num 2) .* Num 4
d2 = Num 3 .* (Num 4 ./ Num 5)
d3 = (Num 4 ./ Num 7) .* (Num 9 ./ Num 3)
d4 = Num 4 ./ Num 8
d5 = (Num 4 .+ Num 5) ./ Num 8

h = (F (Sin (Num 4 .* x)))
c = Num 3 .* x .^ Num 7 .* x .^ Num 2
h1 = c .* h
h2 = h1 ./ (Num 6 .* x .^ Num 4 .+ Num 8 .- x )
h3 = (c ./ (Num 6 .* x .^ Num 4 .+ Num 8 .- x)) .* h

isTrig :: Expr -> Bool
isTrig f = isSin f || isCos f || isTan f || isCsc f || isSec f || isCot f

isInvTrig :: Expr -> Bool
isInvTrig f = isArcsin f || isArccos f || isArctan f
    || isArccsc f || isArcsec f || isArccot f

isHyp :: Expr -> Bool
isHyp f = isSinh f || isCosh f || isTanh f || isCsch f || isSech f || isCoth f

isInvHyp :: Expr -> Bool
isInvHyp f = isArcsinh f || isArccosh f || isArctanh f
    || isArccsch f || isArcsech f || isArccoth f

isLogar :: Expr -> Bool
isLogar f = isLog f || isE f || isLn f

isFunction :: Expr -> Bool
isFunction (F _) = True
isFunction _ = False



-- puts an expression inside a function
push :: Expr -> Expr -> Expr
push newExpr (F f) = F $ fmap (\oldExpr -> newExpr) f
psuh _ e = e


-- testing negExplicit (distribute (negExplicit e)) = negExplicit e
-- testing distribute (neg (distribute e)) = distribute e
-- postcondition:
-- 1) distributes minus signs
-- 2) distributes neg signs
-- 3) distributes (separable) * (separable): (x + 1)^3 replic (x+1)(x+1)(x+1) and is passed
-- to distribute case (Mul a (Mul b c)) where a, b, c are separable and Mul a b  [sep].
distribute :: Expr -> Expr
distribute expr
    | expr' == expr = expr
    | otherwise = distribute expr'
    where
    expr' = dist expr
    dist (Num n) = Num n
    dist (Frac f) = Frac f
    dist (Var x) = Var x
    dist (F f) = F f
    dist (Neg (Num n)) = Num (-n)
    dist (Neg (Frac f)) = Frac (-f)
    dist (Neg e)
        | isAdd e = (dist (Neg a)) .- (dist b)
        | isSub e = (dist (Neg a)) .+ (dist b)
        | isMul e = (dist (Neg a)) .* (dist b) --otherwise = Neg (dist e)
        | isDiv e = (dist (Neg a)) ./ (dist b)
        | otherwise = Neg $ dist e
        where (a, b) = (left e, right e)
    --- note the negative distribute cases
    dist (Sub a (Sub b c)) = (dist a .- dist b) .+ (dist c)
    dist (Sub a (Add b c)) = (dist a .- dist b) .- (dist c)
    dist (Add a (Sub b c)) = (dist a .+ dist b) .- (dist c)
    dist (Add a (Add b c)) = (dist a .+ dist b) .+ (dist c)
    --- note the (x+1)(x+2) distribute cases
    dist (Mul a b)
        | isSeparable a && isSeparable b = 
    dist (Add a b) = Add (dist a) (dist b)
    dist (Sub a b) = Sub (dist a) (dist b)
    dist (Mul a b) = Mul (dist a) (dist b)
    dist (Div a b) = Div (dist a) (dist b)
    dist (Pow a b) = Pow (dist a) (dist b)


clean :: Expr -> Expr
clean expr
    | expr' == expr = expr
    | otherwise = cln expr'
    where
    expr' = cln expr
    fracZero = Frac (Rate 0)
    fracOne = Frac (Rate 1)
    cln (Num n) = Num n
    cln (Frac f) = Frac f
    cln (Var x) = Var x
    cln (F f) = F $ fmap cln f
    cln (Add (Frac (Rate 0)) e2) = cln e2
    cln (Add (Num 0) e2) = cln e2
    cln (Add e1 (Frac (Rate 0))) = cln e1
    cln (Add e1 (Num 0)) = cln e1
    cln (Sub (Frac (Rate 0)) e2) = Neg $ cln e2
    cln (Sub (Num 0) e2) = Neg $ cln e2
    cln (Sub e1 (Frac (Rate 0))) = cln e1
    cln (Sub e1 (Num 0)) = cln e1
    cln (Mul (Frac (Rate 1)) e2) = cln e2
    cln (Mul (Num 1) e2) = cln e2
    cln (Mul e1 (Frac (Rate 1))) = cln e1
    cln (Mul e1 (Num 1)) = cln e1
    cln (Mul (Frac (Rate 0)) e2) = Num 0
    cln (Mul (Num 0) e2) = Num 0
    cln (Mul e1 (Frac (Rate 0))) = Num 0
    cln (Mul e1 (Num 0)) = Num 0
    cln (Div (Num n) (Num m)) = Frac (Rate (n % m))
    cln (Div e1 (Frac (Rate 0))) = error "div by zero"
    cln (Div e1 (Num 0)) = error "div by zero"
    cln (Div (Frac (Rate 0)) e2) = Num 0
    cln (Div (Num 0) e2) = Num 0
    cln (Div e1 (Frac (Rate 1))) = cln e1
    cln (Div e1 (Num 1)) = cln e1
    cln (Pow e1 (Frac (Rate 0))) = Num 1
    cln (Pow e1 (Num 0)) = Num 1
    cln (Pow e1 (Frac (Rate 1))) = cln e1
    cln (Pow e1 (Num 1)) = cln e1 --- TODO do power rules next
    cln (Pow (Frac (Rate 0)) e2) = Num 0
    cln (Pow (Num 0) e2) = Num 0
    cln (Pow (Frac (Rate 1)) e2) = Num 1
    cln (Pow (Num 1) e2) = Num 1

    -- note the neg pusher cases
    cln (Neg (Num n)) = Num (-n)
    cln (Neg (Frac f)) = Frac (-f)
    cln (Neg (Neg e)) = cln e
    cln (Neg e) = Neg $ cln e

    cln (Add e1 e2) = cln e1 .+ cln e2
    cln (Sub e1 e2) = cln e1 .- cln e2
    cln (Mul e1 e2) = cln e1 .* cln e2
    cln (Div e1 e2) = cln e1 ./ cln e2
    cln (Pow e1 e2) = cln e1 .^ cln e2



-- TODO
-- genius:
-- foldl1 (\acc y -> if isNum y then (acc .* y) else acc)    (unGlue e1)

-- note passed a thing like 4xsinxcosx4x^2tanxsecx(3)(2) and result is: 96sinxcosx(x^2)tanxsecx
-- where we write sweep isNum arg to get result.
-- precondition: the expr has to be glued! No add or subtract, just multiply and power.
-- NOTE todo does not work if there are divisions... we just multiply. (look at unglue todo)
-- IDEA or maybe we just use for this multiple trig functions and simplifying general x^2sinx functions
-- before codifying them in codifyOther
-- help don't know whether to simplify expr first or to pass to sweep.

{-
sweep :: (Expr -> Bool) -> Expr -> Expr
sweep f expr =
    | hasDiv expr =
    | hasMul expr =
    where
    explicitDiv = makeDivExplicit expr
    explicitMul = makeMulExplicit expr
-}


{-
glue $ [itemSwept] ++ remainder
    where
    elements = split MulOp expr
    itemSwept = simplifyComplete $ foldl1 (\acc y -> if f y then (acc .* y) else acc) elements
    remainder = filter (not . f) elements
    -- TODO e4 breaks at foldl1 because 2 is ignored.. FIX.
-}



sameArgs :: Function Expr -> Function Expr -> Bool
sameArgs f g
    | (getArg (F f)) == (getArg (F g)) = True
    | otherwise = False


getFunction :: Expr -> Function Expr
getFunction (F f) = f
getFunction _ = error "not a function!"


getArg :: Expr -> Expr
getArg (F (Sin u)) = u
getArg (F (Cos u)) = u
getArg (F (Tan u)) = u
getArg (F (Csc u)) = u
getArg (F (Sec u)) = u
getArg (F (Cot u)) = u
getArg (F (Arcsin u)) = u
getArg (F (Arccos u)) = u
getArg (F (Arctan u)) = u
getArg (F (Arccsc u)) = u
getArg (F (Arcsec u)) = u
getArg (F (Arccot u)) = u
getArg (F (Sinh u)) = u
getArg (F (Cosh u)) = u
getArg (F (Tanh u)) = u
getArg (F (Csch u)) = u
getArg (F (Sech u)) = u
getArg (F (Coth u)) = u
getArg (F (Arcsinh u)) = u
getArg (F (Arccosh u)) = u
getArg (F (Arctanh u)) = u
getArg (F (Arccsch u)) = u
getArg (F (Arcsech u)) = u
getArg (F (Arccoth u)) = u
getArg (F (E u)) = u
getArg (F (Ln u)) = u
getArg (F (Log u v)) = v -- TODO fix so we can get both or handle log separately.


{-

getArg :: Function -> Expr
getArg (Sin u) = u
getArg (Cos u) = u
getArg (Tan u) = u
getArg (Csc u) = u
getArg (Sec u) = u
getArg (Cot u) = u
getArg (Arcsin u) = u
getArg (Arccos u) = u
getArg (Arctan u) = u
getArg (Arccsc u) = u
getArg (Arcsec u) = u
getArg (Arccot u) = u
getArg (Sinh u) = u
getArg (Cosh u) = u
getArg (Tanh u) = u
getArg (Csch u) = u
getArg (Sech u) = u
getArg (Coth u) = u
getArg (Arcsinh u) = u
getArg (Arccosh u) = u
getArg (Arctanh u) = u
getArg (Arccsch u) = u
getArg (Arcsech u) = u
getArg (Arccoth u) = u
getArg (E u) = u
getArg (Ln u) = u
getArg (Log u v) = v -- TODO fix so we can get both or handle log separately.
-}

{-
TODO idea then after we state how functions should simplify, do foldl1 (simpFunc . MUl) over the list.
help

-- HELP TODO use partition .. ??? how to simplify then?
--      1) first unglue, then partition into lists. Separate functions on one side and other stuff
 on other side.
--      2) plan idea: simplify csc, sec, cot in terms of sin,cos, tan.
--      3) count how many sin, cos then change num tans so that 1 sin*cos = 1 tan
--      4) do the same idea for hyperbolics (but depends on their different equality)
-- note if all functions have the same argument then they are simplified, else returned as is.
-- example takes sin * cos * tan * sec  and returns sin^2 x / cos x
-- example takes sin * cos * tan and reurns tan^2 x
-- note multiplication is left associative so we have (((sin) * cos ) * tan)
-- note don't have to handle things like sin * sin because the mul case in simplify does it.
simplifyFunctions :: Expr -> Expr
simplifyFunctions (Mul f@(F (Sin u)) (g@F (Cos v)))
    | sameArgs f g = F $ Tan u
    | otherwise = simplify f .* simplify g
simplifyFunctions (Mul f@(F (Cos u)) (g@F (Sin v)))
    | sameArgs f g = F $ Tan u
    | otherwise = simplify f .* simplify g
simplifyFunctions (Mul f@(F (Sin u)) (g@F (Cos v)))
    | sameArgs f g = F $ Tan u
    | otherwise = simplify f .* simplify g
simplifyFunctions (Mul f@(F (Sin u)) (g@F (Cos v)))
    | sameArgs f g = F $ Tan u
    | otherwise = simplify f .* simplify g
-}


-- for testing chisel.

testChisel = testMM' && testMD' && testDM' && testDD'


mm1' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num (-22) .* (F (Sin x)) .* (F (Cos x))
mm2' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num 22)) .* (F (Sin x))
mm3' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num (-22)
mm4' = Num 7 .* Num 8 .* x .^ Num (-22)
mm5' = Num 7 .* x .^ (Num (-22)) .* (F (Sin x)) .* (F (Cos x)) .* Num 8
mm6' = Num 7 .* x .^ (Num (-22)) .* (F (Sin x)) .* x .^ Num 2


testMM1' = (show $ chisel mm1') == "{((7x(8))sin(x)cos(x)) / ((x + 1)^22)}"
testMM2' = (show $ chisel mm2') == "{((7x(8))sin(x)) / ((x + 1)^22)}"
testMM3' = (show $ chisel mm3') == "{(7x(8)) / ((x + 1)^22)}"
testMM4' = (show $ chisel mm4') == "{(7(8)) / (x^22)}"
testMM5' = (show $ chisel mm5') == "{(7sin(x)cos(x)(8)) / (x^22)}"
testMM6' = (show $ chisel mm6') == "(7x^(-22))(sin(x))(x^2)"

testMM' = testMM1' && testMM2' && testMM3' && testMM4' && testMM5' && testMM6'


-- div div
dd1' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num 22)))) ./ ((F (Sin x)) .- (F (Cos x)))
dd2' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num 22)))) ./ (F (Sin x))
dd3' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num 22))))
dd4' = (Num 7 .+ x .* Num 8) ./ (x .^ Num (-22))
dd5' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num 22)))) ./ ((F (Sin x)) .* (F (Cos x)) .* x .* Num 2)

testDD1' = (show $ chisel dd1') == "{((7 + x(8))(x^22)) / (sin(x) - cos(x))}"
testDD2' = (show $ chisel dd2') == "{((7 + x(8))(x^22))/sin(x)}"
testDD3' = (show $ chisel dd3') == "(7 + x(8))(x^22)"
testDD4' = (show $ chisel dd4') == "(7 + x(8))(x^22)"
testDD5' = (show $ chisel dd5') == "{((7 + x(8))(x^22)) / (sin(x)cos(x)x(2))}"

testDD' = testDD1' && testDD2' && testDD3' && testDD4' && testDD5'


-- mul div
md1' = Num 7 .* x .* (Num 8 ./ ((x .+ Num 1) .^ Num (-22))) .* (F (Sin x)) .* (F (Cos x) .* (F (Tan x)))
md2' = Num 7 .* x .* (Num 8 ./ ((x .+ Num 1) .^ Num (-22))) .* (F (Sin x)) .* (F (Cos x))
md3' = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ (Neg (Num 22))) .* (F (Sin x))
md4' = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ Num (-22))
md5' = Num 7 .* Num 8 ./ (x .^ (Neg (Num 22)))
md6' = Num 7 ./ (x .^ Num (-22))
md7' = x .^ Num (-22)

testMD1' = (show $ chisel md1') == "(7x(8))sin(x)cos(x)tan(x)((x + 1)^22)"
testMD2' = (show $ chisel md2') == "(7x(8))sin(x)cos(x)((x + 1)^22)"
testMD3' = (show $ chisel md3') == "(7x(8))sin(x)((x + 1)^22)"
testMD4' = (show $ chisel md4') == "7x(8)((x + 1)^22)"
testMD5' = (show $ chisel md5') == "7(8)(x^22)"
testMD6' = (show $ chisel md6') == "7x^22"
testMD7' = (show $ chisel md7') == "{(1) / (x^22)}"

testMD' = testMD1' && testMD2' && testMD3' && testMD4' && testMD5' && testMD6' && testMD7'


-- div mul
dm1' = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num (-22)) ./ ( (F (Sin x)) .* (F (Cos x)))
dm2' = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num (-22)) ./ ( (F (Sin x)) )
dm3' = (Num 8 .* (x .+ Num 1) .^ (Neg (Num 22))) ./ ( (F (Sin x)) .* (F (Cos x)))
dm4' = ((x .+ Num 1) .^ Neg (Num 22)) ./ ( (F (Sin x)) .* (F (Cos x)))
dm5' = ((x .+ Num 1) .^ (Neg (Num 22))) ./ ( (F (Sin x)) )

testDM1' = (show $ chisel dm1') == "{(7x(8)) / (((x + 1)^22)sin(x)cos(x))}"
testDM2' = (show $ chisel dm2') == "{(7x(8)) / (((x + 1)^22)sin(x))}"
testDM3' = (show $ chisel dm3') == "{(8) / (((x + 1)^22)sin(x)cos(x))}"
testDM4' = (show $ chisel dm4') == "{(1) / (((x + 1)^22)sin(x)cos(x))}"
testDM5' = (show $ chisel dm5') == "{(1) / (((x + 1)^22)sin(x))}"

testDM' = testDM1' && testDM2' && testDM3' && testDM4' && testDM5'


-- trivial
mm1 = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num 22 .* (F (Sin x)) .* (F (Cos x))
mm2 = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num 22 .* (F (Sin x))
mm3 = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num 22
mm4 = Num 7 .* Num 8 .* x .^ Num 22
mm1'' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22))) .* (F (Sin x)) .* (F (Cos x))
mm2'' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22))) .* (F (Sin x))
mm3'' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22)))
mm4'' = Num 7 .* Num 8 .* x .^ (Neg (Num (-22)))
---
dd1 = ((Num 7 .+ x .* Num 8) ./ (x .^ Num 22)) ./ ((F (Sin x)) .- (F (Cos x)))
dd2 = ((Num 7 .+ x .* Num 8) ./ (x .^ Num 22)) ./ (F (Sin x))
dd3 = ((Num 7 .+ x .* Num 8) ./ (x .^ Num 22))
dd4 = ((Num 7 .+ x .* Num 8) ./ (x .^ Num 22))
dd1'' = (Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num (-22)))) ./ ((F (Sin x)) .- (F (Cos x)))
dd2'' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num (-22))))) ./ (F (Sin x))
dd3'' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num (-22)))))
dd4'' = (Num 7 ./ (x .^ (Neg (Num (-22)))))
---
md1 = Num 7 .* x .* (Num 8 ./ ((x .+ Num 1) .^ Num 22)) .* (F (Sin x)) .* (F (Cos x))
md2 = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ Num 22) .* (F (Sin x))
md3 = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ Num 22)
md4 = Num 7 .* Num 8 ./ (x .^ Num 22)
md5 = Num 7 ./ (x .^ Num 22)
md1'' = Num 7 .* x .* (Num 8 ./ ((x .+ Num 1) .^ (Neg (Num (-22))))) .* (F (Sin x)) .* (F (Cos x))
md2'' = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ (Neg (Num (-22)))) .* (F (Sin x))
md3'' = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ (Neg (Num (-22))))
md4'' = Num 7 .* Num 8 ./ (x .^ (Neg (Num (-22))))
md5'' = Num 7 ./ (x .^ (Neg (Num (-22))))
---
dm1 = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num 22) ./ ( (F (Sin x)) .* (F (Cos x)))
dm2 = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num 22) ./ ( (F (Sin x)) )
dm3 = (Num 8 .* (x .+ Num 1) .^ Num 22) ./ ( (F (Sin x)) .* (F (Cos x)))
dm4 = ((x .+ Num 1) .^ Num 22) ./ ( (F (Sin x)) .* (F (Cos x)))
dm5 = ((x .+ Num 1) .^ Num 22) ./ ( (F (Sin x)) )
dm1'' = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22)))) ./ ( (F (Sin x)) .* (F (Cos x)))
dm2'' = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22)))) ./ ( (F (Sin x)) )
dm3'' = (Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22)))) ./ ( (F (Sin x)) .* (F (Cos x)))
dm4'' = ((x .+ Num 1) .^ (Neg (Num (-22)))) ./ ( (F (Sin x)) .* (F (Cos x)))
dm5'' = ((x .+ Num 1) .^ (Neg (Num (-22)))) ./ ( (F (Sin x)) )
