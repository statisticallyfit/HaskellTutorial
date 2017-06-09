{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleInstances #-}
module Types where


import GHC.Exts (Constraint)
import Control.Monad hiding (join)
import Control.Applicative hiding (Const)
import Data.Char
import Numeric -- math library
import Data.Maybe
import Data.List
import Data.Ratio hiding (show)


-- TODO use record type declaration instead and maybe class for function so no parameter.
data Function a
    = Sin a | Cos a | Tan a |
      Csc a | Sec a | Cot a |
      Arcsin a | Arccos a | Arctan a |
      Arccsc a | Arcsec a | Arccot a |
      Sinh a | Cosh a | Tanh a |
      Csch a | Sech a | Coth a |
      Arcsinh a | Arccosh a | Arctanh a |
      Arccsch a | Arcsech a | Arccoth a |
      Ln a | Exp a | Log a a -- note: first expr is base
    deriving (Eq)

data Op = AddOp | SubOp | MulOp | DivOp | PowOp deriving (Eq)


type Fraction = Ratio Int
data Const = Integer Int | Quotient Fraction deriving (Eq)
-- TODO: how to make Quotient both simplify arg immediately and have numer, denom readily
-- todo accessible, instead of calling Data.Ratio's numerator and denominator functions?

data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
    | Pow Expr Expr | Neg Expr | Num Const | Var String | Func (Function Expr)
    deriving (Eq)
    -- Num Int | Frac Fraction







{-

data Code = Mono (Const, Const) -- monomial (coeff, pow)
    | Poly [Const] -- a string of monomials added together (never multiplied)
    | Trig [Vignette] | InvTrig [Vignette]
    | Hyperbolic [Vignette] | InvHyperbolic [Vignette]
    | {-Exponential Expr |-} Logarithmic (Vignette, Vignette) -- holds ln and log
    -- | Empty -- just a Nil placeholder for the addPoly function, etc.
    deriving (Eq, Show)
-}

-- NOTE: defining the types with typeclasses instead like above because I want each
-- note - "Code" type to be separate. instead of just having add :: Code -> Code -> Code
-- note - we have instead separate, shorter functions for each type: addPoly :: Polynomial -> ...
-- note - and then we get to have them all as type Encoded which has defining template functions:
-- note - add,mul,div.



{-

data Encoded a = Code a {
    add :: a -> a -> a,
    multiply :: a -> a -> a,
    divide :: a -> a -> a
} deriving (Eq, Show)
-}

class Encoded c  where
    add :: c -> c -> c
    multiply :: c -> c -> c
    divide :: c -> c -> c


-- type Vignette c = (Encoded c, Encoded c) -- type synonym for readability -- TODO remove Encoded? leave c?

data Zero = Zero deriving (Eq)
data Monomial = Mono (Const, Const) deriving (Eq, Show)
data Polynomial = Poly [Const] deriving (Eq, Show)
data Trigonometric c = Trig [(c, c)] | InvTrig [(c, c)] deriving (Eq, Show)
data Hyperbolic c = Hyper [(c, c)] | InvHyper [(c, c)] deriving (Eq, Show)
data Logarithmic c = LogBase c c deriving (Eq, Show)

instance Show Zero where
    show Zero = "0"


-- NOTE: instances are declared in Codes.hs file.



------------------------------------------------------------------------------------------------
-- Op Instances


instance Show Op where
    show AddOp = "(+)"
    show SubOp = "(-)"
    show MulOp = "(*)"
    show DivOp = "(/)"
    show PowOp = "(^)"


------------------------------------------------------------------------------------------------


instance  {-# OVERLAPPING #-} Show Fraction where
    show ratio
        | numerator ratio == 0 = "0"
        | denominator ratio == 1 = show (numerator ratio)
        | otherwise = (show (numerator ratio)) ++ "/" ++ (show (denominator ratio))



------------------------------------------------------------------------------------------------
-- Const Instances



intToConst :: Int -> Int -> Const
intToConst num denom
    | denominator f == 1 = Integer (numerator f)
    | otherwise = Quotient f
    where f = num % denom


fracToConst :: Fraction -> Const
fracToConst f
    | denominator f == 1 = Integer $ numerator f
    | otherwise = Quotient f



instance Num Const where
    negate (Integer int) = Integer $ negate int
    negate (Quotient frac) = Quotient $ negate frac

    (Integer x) + (Integer y) = Integer (x + y)
    (Integer x) + (Quotient y) = fracToConst (x % 1 + y)
    (Quotient x) + (Integer y) = fracToConst (x + y % 1)
    (Quotient x) + (Quotient y) = fracToConst (x + y)

    (Integer x) * (Integer y) = Integer (x * y)
    (Integer x) * (Quotient y) = fracToConst (x % 1 * y)
    (Quotient x) * (Integer y) = fracToConst (x * (y % 1))
    (Quotient x) * (Quotient y) = fracToConst (x * y)

    fromInteger num = Integer $ fromInteger num

    abs (Integer int) = Integer $ abs int
    abs (Quotient q) = Quotient $ abs q

    signum (Integer int) = Integer $ signum int
    signum (Quotient q) = Quotient $ signum q



instance Fractional Const where
    (Integer a) / (Integer b) = fracToConst (a % b)
    (Integer a) / (Quotient b) = fracToConst ((a % 1) * b)
    (Quotient a) / (Integer b) = fracToConst (a * (b % 1))
    (Quotient a) / (Quotient b) = fracToConst (a / b)

    recip (Integer x) = Quotient (1 % x)
    recip (Quotient q) = Quotient (recip q)

    fromRational rat = Quotient ((fromInteger $ numerator rat) % (fromInteger $ denominator rat))


instance Ord Const where
    compare (Integer x) (Integer y) = compare x y
    compare (Quotient a) (Quotient b) = compare a b


instance Show Const where
    show (Integer int) = show int
    show (Quotient q) = show q ++ ""


------------------------------------------------------------------------------------------------
-- Function Instances

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
    fmap f (Exp x) = Exp (f x)
    fmap f (Ln x) = Ln (f x)
    fmap f (Log base x) = Log (f base) (f x)


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
    show (Exp e) = "e^(" ++ show e ++ ")"
    show (Ln e) = "ln(" ++ show e ++ ")"
    show (Log b a) = "log" ++ show b ++ "(" ++ show a ++ ")"

------------------------------------------------------------------------------------------------
-- Expr Instances

instance Show Expr where
    show (Var x) = x
    show (Num const) = show const
    show (Func func) = show func
    show (Neg e) = "-(" ++ show e ++ ")"
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2) = show e1 ++ " - (" ++ show e2 ++ ")"
    show (Mul e1 e2) = show e1 ++ show e2
    show (Div e1 e2) = "(" ++ show e1 ++ ") / (" ++ show e2 ++ ")"
    show (Pow base exp) = show base ++ "^(" ++ show exp ++ ")"

------------------------------------------------------------------------------------------------


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


