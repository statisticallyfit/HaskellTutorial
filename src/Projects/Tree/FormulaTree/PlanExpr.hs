module ExprPlan where

import Test.QuickCheck -- (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad hiding (join)
import Control.Applicative
import Data.Char
import Numeric
import Data.Maybe
import Data.List


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

data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
    | Pow Expr Expr | Neg Expr | Num Int | Var String | F (Function Expr)
    deriving (Eq)

type Coeff = Int
type Description = (Expr, Expr, Expr)

data Code = Poly [Coeff] | PolyDiv [Coeff] [Coeff] | Trig [Description] | InvTrig [Description]
    | Hyperbolic [Description] | InvHyp [Description] | Logarithmic [Description]
    deriving (Eq, Show)



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
    show (Neg (Num n))
        = if (n < 0)
        then ("-(" ++ show n ++ ")")
        else ("-" ++ show n)
    show (F func) = show func
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2) = show e1 ++ " - " ++ show e2

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
    show (Mul maybeMono (F f))
        | isMono maybeMono = "(" ++ show maybeMono ++ ")" ++ show f
        | otherwise = show maybeMono ++ show f
    show (Mul m1@(Mul _ _) m2@(Mul _ _))
        | many m1 && many m2 = "(" ++ show m1 ++ ") (" ++ show m2 ++ ")"
        | many m1 = "(" ++ show m1 ++ ") " ++ show m2
        | many m2 = show m1 ++ " (" ++ show m2 ++ ")"
        | otherwise = show m1 ++ show m2
        where many m = (length $ splitAS m) > 1
    show (Mul e1 ng@(Neg (Num n)))
        = if (n >= 0)
        then (show e1 ++ "(-" ++ show n ++ ")")
        else (show e1 ++ "(-(-" ++ show (-1*n) ++ "))")

    show (Mul e1 e2)
        | many1 && many2 = "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"
        | many1 = "(" ++ show e1 ++ ")" ++ show e2
        | many2 = show e1 ++ "(" ++ show e2 ++ ")"
        | otherwise = show e1 ++ show e2
        where
        many1 = isPow e1 || many e1
        many2 = isPow e2 || isNum e2 || many e2
        many e = (length $ splitAS e) > 1

    show (Div e1 e2)
        | few e1 && few e2 = surround $ show e1 ++ "/" ++ show e2
        | few e1 = surround $ show e1 ++ "/(" ++ show e2 ++ ")"
        | few e2 = surround $ "(" ++ show e1 ++ ")/" ++ show e2
        | otherwise = surround $ "(" ++ show e1 ++ ") / (" ++ show e2 ++ ")"
        where
        surround eStr = "{" ++ eStr ++ "}"
        few e = (isVar e || isNum e || (not $ isMono e)) && ((not $ isNumNeg e) && (not $ isNegNum e)
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
        | few e1 && few e2 = show e1 ++ "^" ++ show e2
        | few e1 = show e1 ++ "^(" ++ show e2 ++ ")"
        | few e2 = "(" ++ show e1 ++ ")^" ++ show e2
        | otherwise = "(" ++ show e1 ++ ")^(" ++ show e2 ++ ")"
        where
        few e = isVar e || isNum e || hasNeg e

    show (Neg m@(Mul _ _)) = "-" ++ show m
    show (Neg d@(Div _ _)) = "-" ++ show d
    show (Neg p@(Pow _ _)) = "-" ++ show p
    show (Neg f@(F _)) = "-" ++ show f
    show (Neg (Var x)) = "-" ++ x
    show (Neg e) = "-(" ++ show e ++ ")"

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
    (Num 8 .* x .^ Num 3)
e24 = (Num 3 .* x .^ Num (-2) .* Num 4 .* x .^ Num 3 .+
    Num 3 .* x .^ Num (-8) .* Num 5 .* x .^ Num 4 .* (x .+ Num 1) .^ Num 6) ./
    (Num 8 .* x .^ Num 3)
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
split :: Op -> Expr -> [Expr]
split _ (Var x) = [Var x]
split _ (Num n) = [Num n]
split _ (F f) = [F f]
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
    | isDiv e && hasMul e = if (not $ isMul expl) then [expl] else (split MulOp expl)
    -- | hasAdd e || hasSub e = [e] -- note respecting order of operations
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
    | isMul e && hasDiv e = if (not $ isDiv expl) then [expl] else (split DivOp expl)
    -- | hasAdd e || hasSub e = [e] -- note respecting order of operations
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
pickMethod _ _  = error "only ops are: add, sub, mul, div"

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


---------------------------------------------------------

-- note converts expression to code
-- first splits expr at the plus / minus signs and then categorizes each element as either
-- function or polynomial term. Puts them into code accordingly.
-- example If we have 7x^2 and 3x^2 in the same expr, then we have: Poly [0,0,(7+3)
-- NOTE TODO if we have 3sec^2 x + sinxcosxtanx + x^2 tan x then
-- we split: [3sec^2 x, sinxcosxtanx, x^2tan x]
-- THen decide: map (decide if more than one function) over this
-- then if not we send partition those that have more than one function and those that do not.
-- For those that do not: send to simplify or rearrangeconst and to codifying to be simplified
-- in case of repeat things, like we can have 4sec^2x that we must add to get 7sec^2x...
-- For those that do have more than 1 func: send to functionsimplifier.
-- TODO help filtering is incorrect here. Learn to allow things like x^2 sinx through the filter as well
-- not just pure trig or hyp .. functions.
{-
simplifyExpr :: Expr -> Expr
simplifyExpr expr = ps' .+ fs'
    where
    --- TODO remember to chisel!
    (ps, fs) = partition isMono (splitAS expr)
    (gs, ggs) = partition hasOnlyOneFunction fs
    ps' = rebuildAS $ decodifyPoly $ codifyPoly ps
    gs' = rebuildAS $ map decodifySingleFunction $ codifySingleFunctions gs
    ggs' = simplifyFunctions ggs
    fs' = gs' .+ ggs'
-}




{-exprToCode :: Expr -> Code
exprToCode expr = Code $ filter (not . emptyGroup) [poly, trig, invTrig, hyp, invHyp, logs]
    where
    ss = split expr
    poly = codifyPoly $ filter isPoly ss
    trig = codifyOther $ filter isTrig ss
    invTrig = codifyOther $ filter isTrig ss
    hyp = codifyOther $ filter isTrig ss
    invHyp = codifyOther $ filter isTrig ss
    logs = codifyOther $ filter isTrig ss

    emptyGroup (Poly ps) = null ps
    emptyGroup (Trig ts) = null ts
    emptyGroup (InvTrig is) = null is
    emptyGroup (Hyperbolic hs) = null hs
    emptyGroup (InvHyp is) = null is
    emptyGroup (Logarithmic ls) = null ls-}

-- note
-- precondition: an expression like 3sec^2x + sinx * cosx * tanx + x^2tan^2(x) will be separated
-- from the glued and non-glued expressions. THen this function is passed the non-glued expressions,
-- whereas the glued expressions like sinxcosxtanx will get passed to simplifyFunction then both
-- come together in a simplifier function that  rebuilds the two expression lists.
-- precondition: so in short, this functino is not prepared for things like sinx * cos x * tan x
{-
codifyOther :: [Expr] -> Group
codifyOther [] = Tr
-}

-- TODO simplifier function that uses codifyOther and exprToCode must pass the sinxcosxtanx
-- as trailing thread then rebuild the nonglued expressions with the above glued expressions
-- similar to rebuild split thing.  Remember to learn to separate by calling "hasMoreThanOneFunc"


-- example: if functino is in "cos" family (arccos, arccosH, cos, cosh), then it gets put in certain
-- location: sin, cos,tan, csc, sec, cot
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
    | otherwise = error "unknown expression-function"


-- note takes a codeified polynomial, or trig or invtrig... and adds the two codified things.
-- note todo currently just impleneted for codified polynomial
-- postcondition: get somethning like (4x^5)(8x^2)sinx^6 and
{-
codifySingleFunction :: Expr -> Code
codifySingleFunction expr
    | isTrig f = Trig codes
    | isInvTrig f = InvTrig codes
    | isHyp f = Hyperbolic codes
    | isInvHyp f = InvHyp codes
    | isLogar f = Logarithmic codes
    where
    -- note simplifying the polynomial part
    (ps, (f:_)) = partition isMono (split MulOp expr)
    dmp = chisel (rebuild MulOp ps)
    ps' = if (isDiv dmp) then (codifyPolyD dmp) else (codifyPolyM dmp)
    (low, upper) = if (isDiv ps') then (getLower ps', getUpper ps') else ps'
    coef = rebuild MulOp $ decodifyMulPoly $ foldl1 mulPoly (map (\p -> codifyAddPoly [p]) ps')
    -- note now actually making it a code (always 6 spots since: sin,cos,tan,csc,sec,cot are 6)
    -- but there are 3 for log zs
    descr = (coef, getPow f, getArg f)
    zs = replicate 6 (Num 0, Num 0, Num 0)
    zsLog = replicate 3 (Num 0, Num 0, Num 0)
    codes = if (isLogFamily f) then (put (findLoc f) descr zsLog) else (put (findLoc f) descr zs)
-}


{-
TODO continue tomorrow here !!!! @ !!!!!
Need to make a divPoly function for cases like (x + x^2 + 3x^2(4x^7)(5x^3)) / x^7
*ExprPlan> let (ps, (f:_)) = partition isMono (split MulOp e16)
*ExprPlan> ps
[2,8,2,x^(-7),3,x]
*ExprPlan> f
sin(x^(-8))
*ExprPlan> let temp = rebuild MulOp ps
*ExprPlan> temp
2(8)(2)(x^(-7))(3)x
*ExprPlan> hasNegPow it
True
*ExprPlan> makeDivExplicit tempPs

<interactive>:855:17:
    Not in scope: `tempPs'
    Perhaps you meant `temp' (line 852)
*ExprPlan> makeDivExplicit temp
 [(2(8)(2)(3)x) / (x^7)]
*ExprPlan> split MulOp it
[2,8,2,3,x, [(1) / (x^7)] ]
*ExprPlan> makeDivExplicit temp
 [(2(8)(2)(3)x) / (x^7)]
*ExprPlan> split DivOp it
[2(8)(2)(3)x,x^7]
*ExprPlan>

-}

-- note gets the coefficient and power of the monomial.
-- precondition: input needs to be a mono
getCoefPowPair :: Expr -> (Int, Int)
getCoefPowPair (Var _) = (1, 1)
getCoefPowPair (Num n) = (n, 0) -- note this is pow 0 of x, won't be applied to the Num n.
getCoefPowPair (Mul (Num n) (Var _)) = (n, 1)
getCoefPowPair (Mul (Neg (Num n)) (Var _)) = (-n, 1)
getCoefPowPair (Mul (Num n) (Neg (Var _))) = (-n, 1)
getCoefPowPair (Mul (Neg (Num n)) (Neg (Var _))) = (n, 1)
getCoefPowPair (Pow (Var _) (Num p)) = (1, p)
getCoefPowPair (Pow (Var _) (Neg (Num p))) = (1, -p)
getCoefPowPair (Pow (Neg (Var _)) (Num p)) = (-1, p)
getCoefPowPair (Pow (Neg (Var _)) (Neg (Num p))) = (-1, -p)
getCoefPowPair (Mul (Num c) (Pow (Var _) (Num p))) = (c, p)
getCoefPowPair (Mul (Num c) (Pow (Var _) (Neg (Num p)))) = (c, -p)
getCoefPowPair (Mul (Neg (Num c)) (Pow (Var _) (Num p))) = (-c, p)
getCoefPowPair (Mul (Neg (Num c)) (Pow (Var _) (Neg (Num p)))) = (-c, -p)
getCoefPowPair (Neg e) = putNegFirst (getCoefPowPair e)
    where putNegFirst (n,p) = (-n, p)


-- note takes a single monomial and converts it to coded form
-- precondition: no neg powers, output of chisel, AND must be monomial.
codifyMono :: Expr -> Code
codifyMono expr = Poly $ zs ++ [c]
    where
    (c, p) = getCoefPowPair expr
    zs = replicate p 0

-- note takes list of polynomials and makes it into Group type Poly [...]
-- so 7x^2 + 3x^2 + 3x + 4x + 1 is [1, (3+4), (7+3)]
codifyPolyA :: Expr -> Code
codifyPolyA expr = foldl1 addPoly (map codifyMono ps)
    where ps = splitAS expr


-- precondition: takes chisel output so has no negpowers. There is no division (assume) just mul.
-- Ignore expressions that are of form (x+1), just use the monomials.
codifyPolyM :: Expr -> Code
codifyPolyM expr = foldl1 mulPoly (map codifyMono ps)
    where ps = split MulOp expr


-- TODO START HERE TOMORROW
-- precondition: takes chisel output which isDiv and which has no other div in the numerator
-- and denominator and no negpowers. Can have add/sub in numberator though, but simplification
-- is only possible in divpoly if denom has no add/sub.
{-
codifyPolyD :: Expr -> Code
codifyPolyD expr = divPoly (codifyPolyM upper) (codifyPolyM lower)
    where
    lower = getLower (split DivOp expr)
    upper = getUpper (split DivOp expr)
-}



decodifyPoly :: Code -> [Expr]
decodifyPoly (Poly ps) = map clean polynomials
    where
    ns = map Num ps
    xs = replicate (length ps) x
    pows = map Num $ [0 .. (length ps - 1)]
    xs' = map simplifyComplete $ zipWith Pow xs pows
    ps' = map negExplicit $ zipWith Mul ns xs'
    polynomials = [clean (head ps')] ++ tail ps'

    negExplicit (m@(Mul (Num n) p@(Pow _ _))) = if (n < 0) then (Neg $ (Num (-1*n)) .* p) else m
    negExplicit e = e

{-
codifyAddPoly [] = Poly []
codifyAddPoly ps = Poly $ foldl1 (zipWith (+)) $ addZeroes $ codify ps
    where -- note poly expects no forms like 7x / 4x^3 in the list element position
    addZeroes cs = map (\xs -> xs ++ replicate (maxCodeLen - length xs) 0) cs
    codify ps = map poly (map simplify ps)
    maxCodeLen = foldl1 max $ map length (codify ps)
    poly (Var x) = [0, 1]
    poly (Num n) = [n]
    poly (F func) = error "no functions allowed in makePoly"
    -- poly (Pow x (Neg (Num p))) TODO HELP
    poly (Pow x (Num p)) = replicate p 0 ++ [1]
    poly (Mul (Neg (Num n)) (Pow x (Num p))) = replicate p 0 ++ [-n]
    poly (Mul (Neg (Num n)) x) = [0, -n]
    poly (Mul (Num n) (Pow x (Num p))) = replicate p 0 ++ [n]
    poly (Mul (Num n) x) = [0, n]
-}


{-
TODO *** resintate this - don't need a specific decoder for every operation
IDEA



-- not done yet, just copied content from addsub decodifier
-- assumes that we are decoding a polynomial whose elements are multiplied.
decodifyMulPoly :: Code -> [Expr]
decodifyPoly (Poly ps) = polynomials
    where
    ns = map Num ps
    xs = replicate (length ps) x
    pows = map Num $ [0 .. (length ps - 1)]
    xs' = map simplifyComplete $ zipWith Pow xs pows
    ps' = map negExplicit $ zipWith Mul ns xs'
    polynomials = [simplify (head ps')] ++ tail ps'

    negExplicit (m@(Mul (Num n) p@(Pow _ _))) = if (n < 0) then (Neg $ (Num (-1*n)) .* p) else m
    negExplicit e = e
-}




--addCodes :: Code -> Code -> Code
{-
addSingleFunctions (Trig ts) (Trig us) = simplifiedMaybes
    where
    add (a@(c1,p1,x1), b@(c2,p2,x2)) =
        if (x1 == x2 && p1 == p2) then (Just (c1 .+ c2, p1, x1), Nothing)
        else (Just a, Just b)
    simpFirst (x,y,z) = (simplify x, y, z)
    simpMaybe (a,b) =
        if (isJust a && isNothing b) then (Just $ simpFirst $ fromJust a, Nothing)
        else (a,b)
    simplifiedMaybes = map simpMaybe $ map add (zip h1' h2')
-}

list = map numify [(4,1,x), (3,1,x), (1,7,(Num 2) .* x .^ Num 5), (2,2,x),(1,1,x), (7,7,Num 7 .* x)]
add (a@(c1,e1,x1),b@(c2,e2,x2)) = if x1 == x2 then (Just (c1 .+ c2,e1,x1), Nothing) else (Just a, Just b)
hs = zip list list
js = map add hs
clean' (x,y,z) = (clean x, y, z)
ms = map (\(a, b) -> if (isJust a && isNothing b) then (Just $ clean' $ fromJust a, Nothing) else (a,b)) js
--g (a@(c1,e1,x1), b@(c2,e2,x2)) = if (x1 == x2) then (c1 .+ c2,e1,x1) else (a,b)



put :: Int -> a -> [a] -> [a]
put _ n [] = [n]
put index n xs
    | index < 0 = error "index is negative "
    | otherwise = front ++ [n] ++ (tail back)
    where (front, back) = splitAt index xs
          newBack = if null back then [] else (tail back)

elongate :: Int -> [Int] -> [Int]
elongate toLen xs = xs ++ replicate (toLen - (length xs)) 0


-- Note all these functions with poly below assume the ps inside the Poly are added.
addPoly :: Code -> Code -> Code
addPoly (Poly ps) (Poly qs) = Poly (zipWith (+) ps' qs')
    where
    maxLen = max (length ps) (length qs)
    ps' = elongate maxLen ps
    qs' = elongate maxLen qs

subPoly :: Code -> Code -> Code
subPoly (Poly ps) (Poly qs) = Poly (zipWith (-) ps qs)

mulPoly :: Code -> Code -> Code
mulPoly (Poly ps) (Poly qs) = Poly $ foldl1 (zipWith (+)) gs'
    where
    ts = zip ps [0..(length ps - 1)]
    gs = map (\(n, p) -> mulOnePoly n p qs) ts
    gs' = map (\xs -> xs ++ replicate (maxPolyPow - length xs) 0) gs
    maxPolyPow = maximum $ map length gs

-- note n = coeff of poly, p = pow of poly with coeff n, q = pow of multiplied poly (accumulated)
-- (m:ms) = elements of other polynomial (added), acc = accumulated multiplications (is a list of
-- tuples that holds first the new coeff value and second the power of this coeff.
mulOnePoly :: Int -> Int -> [Int] -> [Int]
mulOnePoly n p ms = foldl (zipWith (+)) zs cs
    where
    cs = map (\(c,p) -> put p c zs) ts
    zs = replicate (highestPow + 1) 0
    ts = mul' n p 0 ms []
    highestPow = maximum $ map snd ts
    mul' _ _ _ [] acc = acc
    mul' 0 _ _ _ acc = [(0, 0)] ++ acc
    mul' n p q (m:ms) acc
        | n * m == 0 = mul' n p (q + 1) ms acc
        | otherwise = mul' n p (q + 1) ms (acc ++ [(n * m, p + q)])


-- precondition: takes non-div expression whcih has no negpowers.
-- Can have add/sub in numberator though, but just mul in denominator.
{-
divPoly :: Group -> Group -> Group
divPoly (Poly ps) (Poly qs)
    | moreThanOneTerm = error "more than one added term in bottom; cannot divide"
    | otherwise =
    where
    moreThanOneTerm = length $ filter (\x -> not (x == 0)) qs > 1
-- TODO paused on this because need fraction.
divOnePoly :: Int -> Int -> [Int] -> [Int]
divOnePoly n p ms =
    where
    div' n p q m acc
        |
-}








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


latch zs = map (\((c,e,u), f) -> c .* (push u f) .^ e) (map (\((tup, f)) -> (numify tup, f)) zs)
numify (c,e,u) = (Num c, Num e, u)








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
getUpper _ = error "not div expr in getUpper"

getLower :: Expr -> Expr
getLower (Div _ denom) = denom
getLower _ = error "not div expr in getLower"

isPow :: Expr -> Bool
isPow (Pow _ _) = True
isPow _ = False

getPow :: Expr -> Expr
getPow (Pow base expo) = expo
getPow expr = Num 1 -- if not an actual power, then expo is 1

isNum :: Expr -> Bool
isNum (Num _) = True
isNum _ = False

getNum :: Expr -> Int
getNum (Num n) = n
getNum (Neg (Num n)) = -n

isNegNum :: Expr -> Bool
isNegNum (Neg (Num n)) = True
isNegNum _ = False

isNumNeg :: Expr -> Bool
isNumNeg (Num n) = n < 0
isNumNeg _ = False

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
hasFunction (Neg e) = hasDiv e
hasFunction (Add e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Sub e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Mul e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Div e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Pow e1 e2) = hasFunction e1 || hasFunction e2


hasNeg :: Expr -> Bool
hasNeg (Var _) = False
hasNeg (F _) = False
hasNeg (Num n) = if (n < 0) then True else False
hasNeg (Neg _) = True
hasNeg (Add e1 e2) = hasNeg e1 || hasNeg e2
hasNeg (Sub e1 e2) = hasNeg e1 || hasFunction e2
hasNeg (Mul e1 e2) = hasNeg e1 || hasNeg e2
hasNeg (Div e1 e2) = hasNeg e1 || hasNeg e2
hasNeg (Pow e1 e2) = hasNeg e1 || hasNeg e2


hasNegPow :: Expr -> Bool
hasNegPow (Var _) = False
hasNegPow (F _) = False
hasNegPow (Num n) = if (n < 0) then True else False
hasNegPow (Neg _) = True
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
right (F f) = F f
right (Neg e) = Neg (right e)
right (Add e1 e2) = e2
right (Sub e1 e2) = e2
right (Mul e1 e2) = e2
right (Div e1 e2) = e2
right (Pow e1 e2) = e2




-- rebuilds with add sub signs.
rebuildAS :: [Expr] -> Expr
rebuildAS es = foldl1 f es
    where f acc x
            | isNeg x = (Sub acc (getNeg x))
            | x == Num 0 = acc
            | otherwise = Add acc x

rebuild :: Op -> [Expr] -> Expr
rebuild AddOp es = foldl1 (\acc x -> if (x == Num 0) then acc else (Add acc x)) es
rebuild SubOp es = foldl1 (\acc x -> if (x == Num 0) then acc else (Sub acc x)) es
rebuild MulOp es = foldl1 (\acc x -> Mul acc x) es
rebuild DivOp es = foldl1 (\acc x -> Div acc x) es
rebuild PowOp es = foldl1 (\acc x -> Pow acc x) es



-- START HERE tomorrow 10/14: was working on codifying but first update sweep to save mono
-- IDEA IMPORTANT: make sweep so that it goes into (Add e1 e2) = Add (sweep e1) (sweep e2)
-- and then deal with gathering up constants for glued expressions.

-- need to use sweep constants (make sweep so that
-- note says if expression is a single polynomial term like 5x (monomial)
isMono :: Expr -> Bool
isMono e
    | hasFunction e || ((length $ splitAS e) > 1) = False
    | otherwise = foldl f True s'
    where
    -- e' = simplifyComplete e
    s' = map simplifyComplete $ split MulOp e -- was e' , changed since got sent to infinite loop.
    f = (\acc x -> acc && (isNum x || isVar x || isPolyPow x))
    isPolyPow (Pow (Var _) (Neg (Num n))) = True
    isPolyPow (Pow (Var _) (Num n)) = True
    isPolyPow _ = False


-- note says if expression contains no functions and is just a polynomial term like 7x^2 / 6x or just 5x
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

isTrig :: Expr -> Bool
isTrig f = isSin f || isCos f || isTan f || isCsc f || isSec f || isCot f


-- note this is passed only glued expressions!
-- example 3x^7x^2sin(4x) = True
-- example (sin (sin x)) = True
-- example (sinxtanx) = False
hasOnlyOneFunction :: Expr -> Bool
hasOnlyOneFunction expr = (oneFunc 0 expr) == 1
    where
    oneFunc count (Num n) = count
    oneFunc count (Var x) = count
    oneFunc count (F f) = count + 1
    oneFunc count (Neg e) = oneFunc count e
    oneFunc count (Add e1 e2) = oneFunc count e1 + oneFunc count e2
    oneFunc count (Sub e1 e2) = oneFunc count e1 + oneFunc count e2
    oneFunc count (Mul e1 e2) = oneFunc count e1 + oneFunc count e2
    oneFunc count (Div e1 e2) = oneFunc count e1 + oneFunc count e2
    oneFunc count (Pow e1 e2) = oneFunc count e1 + oneFunc count e2


{-
-- precondition: expression must have no add or sub and must have at least one function.
-- postcondition: takes out first function it finds, leaving the expression otherwise the same.
-- note only meant to deal with functions like x^2 sin(x) ^ (3x) or sinx ^ tan x but not sinxtanx
extractFunction :: Expr -> Expr
extractFunction (Mul (F f) (F g)) = Mul (Num 1) (F g)
extractFunction (Mul (F f) e) = Mul (Num 1) e
extractFunction (Mul e (F g)) = Mul e (Num 1)
extractFunction (Div (F f) (F g)) = Div (Num 1) (F g)
extractFunction (Div (F f) e) = Div (Num 1) e
extractFunction (Div e (F g)) = Div e (Num 1)
extractFunction (Pow (F f) (F g))
extractFunction (Neg (F f)) = Num (-1) -- here putting into play strong assuming of no add and no sub.
extractFunction _ = error "not a function"
-}


-- postcondition: converts negative pow to positive by changing to div or mul.
chisel :: Expr -> Expr
chisel expr
    | (length $ splitAS expr) > 1 = error "expr must not have exterior added/subtracted terms"
    | expr' == expr = makeDivExplicit expr
    | otherwise = chisel expr'
    where
    expr' = if (hasDiv expr) then (chiseler (makeDivExplicit expr)) else (chiseler expr)
    chiseler (Num n) = Num n
    chiseler (Var x) = Var x
    chiseler (Neg e) = Neg $ chiseler e
    chiseler (F f) = F f  -- TODO functor here to map inside and chisel the function args.

    --- note: the ((n/m)/p) simplification cases
    chiseler (Div (Div a b) (Div c d)) = Div (chiseler a .* chiseler d) (chiseler b .* chiseler c)
    chiseler (Div (Div a b) other) = Div (chiseler a) (chiseler b .* chiseler other)
    chiseler (Div other (Div c d)) = Div (chiseler other .* chiseler d) (chiseler c)
    chiseler (Div (Add a b) c)
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
        c' = chiseler c
    ------ note the pow cases.
    -- note keeping these despite making div explicit  because result is (7 * 1) / pow
    -- instead of (7 / pow) and we get the latter if we use the below  (for mm3')
    chiseler m@(Mul a (Pow base (Neg (Num n))))
        | n >= 0 = Div (chiseler a) (Pow (chiseler base) (Num n))
        | otherwise = Mul (chiseler a) (Pow (chiseler base) (Num (-1*n)))
    chiseler m@((Mul a (Pow base (Num n))))
        | n < 0 = Div (chiseler a) (Pow (chiseler base) (Num (-1*n)))
        | otherwise = (chiseler a) .* (chiseler base) .^ Num n
    chiseler d@(Div a (Pow base (Neg (Num n))))
        | n >= 0 = Mul (chiseler a) (Pow (chiseler base) (Num n))
        | otherwise = Div (chiseler a) (Pow (chiseler base) (Num (-1*n)))
    chiseler d@(Div a (Pow base (Num n)))
        | n < 0 = (chiseler a) .* (chiseler base) .^ Num (-1*n)
        | otherwise = Div (chiseler a) (Pow (chiseler base) (Num n))

    chiseler p@(Pow base (Neg (Num n)))
        | n >= 0 = Num 1 ./ ((chiseler base) .^ Num n)
        | otherwise = (chiseler base) .^ Num (-1*n)
    chiseler p@(Pow base (Num n))
        | n < 0 = Num 1 ./ ((chiseler base) .^ (Num (-1*n)))
        | otherwise = (chiseler base) .^ (Num n)


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



clean :: Expr -> Expr
clean expr
    | expr' == expr = expr
    | otherwise = cln expr'
    where
    expr' = cln expr
    cln (Num n) = Num n
    cln (Var x) = Var x
    cln (F f) = F $ fmap cln f
    cln (Neg e) = cln e
    cln (Add (Num 0) e2) = cln e2
    cln (Add e1 (Num 0)) = cln e1
    cln (Sub (Num 0) e2) = Neg $ cln e2
    cln (Sub e1 (Num 0)) = cln e1
    cln (Mul (Num 1) e2) = cln e2
    cln (Mul e1 (Num 1)) = cln e1
    cln (Mul (Num 0) e2) = Num 0
    cln (Mul e1 (Num 0)) = Num 0
    cln (Div e1 (Num 0)) = error "div by zero"
    cln (Div (Num 0) e2) = Num 0
    cln (Div e1 (Num 1)) = cln e1
    cln (Pow e1 (Num 0)) = Num 1
    cln (Pow e1 (Num 1)) = cln e1 --- TODO do power rules next
    cln (Pow (Num 0) e2) = Num 0
    cln (Pow (Num 1) e2) = Num 1
    cln (Add e1 e2)
        | (num e1 && num e2) = (Num $ (getNum e1) + (getNum e2))
        | otherwise = cln e1 .+ cln e2
        where num x = isNegNum x || isNumNeg x || isNum x
    cln (Sub e1 e2)
        | (num e1 && num e2) = (Num $ (getNum e1) - (getNum e2))
        | otherwise = cln e1 .- cln e2
        where num x = isNegNum x || isNumNeg x || isNum x
    cln (Mul e1 e2)
        | (num e1 && num e2) = (Num $ (getNum e1) * (getNum e2))
        | otherwise = cln e1 .* cln e2
        where num x = isNegNum x || isNumNeg x || isNum x
    cln (Div e1 e2)
        | (num e1 && num e2) = (Num $ (getNum e1) `div` (getNum e2))
        | otherwise = cln e1 ./ cln e2
        where num x = isNegNum x || isNumNeg x || isNum x
    cln (Pow e1 e2)
        | (num e1 && num e2) = (Num $ (getNum e1) ^ (getNum e2))
        | otherwise = cln e1 .^ cln e2
        where num x = isNegNum x || isNumNeg x || isNum x


-- TODO major help why doesn't 4(x+3) simplify to 4x + 12, why 4x + (4)(3)?? ?
-- TODO probably because this has gotten too old - perhaps ther eis a case that goes ahead of the
-- one that should be entered that keeps it in this ugly state? Fix with printExpr and foldl.
simplify :: Expr -> Expr
simplify (Var x) = Var x
simplify (Num n) = Num n
simplify (F f) = F $ fmap simplify f

-- TODO may not need these cases once I do operations on expression holders of arrays.
simplify (Add (Num n) (Num m)) = Num $ n + m
simplify (Add (Mul (Num a) (Num b)) (Mul (Num c) (Num d))) = Num $ a * b + c + d
simplify (Add (Mul (Num a) x) (Mul (Num b) y))
    = if x == y then (Num (a + b) .* simplify x) else ((Num a) .* simplify x .+ (Num b) .* simplify y)
simplify (Add (Mul (Num a) x) (Mul y (Num b))) = simplify ((Num a) .* x .+ (Num b) .* y)
simplify (Add (Mul x (Num a)) (Mul (Num b) y)) = simplify ((Num a) .* x .+ (Num b) .* y)
simplify (Add (Mul x (Num a)) (Mul y (Num b))) = simplify ((Num a) .* x .+ (Num b) .* y)
simplify (Add rest (Mul (Num n) (Num m))) = simplify rest .+ (Num $ n * m)
simplify (Add (Neg e1) (Neg e2)) = simplify (Neg e1) .- simplify e2
simplify (Add (Neg e1) e2) = simplify (Neg e1) .+ simplify e2
simplify (Add e1 (Neg e2)) = simplify e1 .- simplify e2
simplify (Add x y) = if x == y then (Num 2 .* simplify x) else (simplify x .+ simplify y)

simplify (Sub (Num n) (Num m)) = Num $ n - m
simplify (Sub (Mul (Num a) (Num b)) (Mul (Num c) (Num d))) = Num $ a * b - c + d
simplify (Sub (Mul (Num a) x) (Mul (Num b) y))
    = if x == y then (Num (a - b) .* simplify x) else ((Num a) .* simplify x .- (Num b) .* simplify y)
simplify (Sub (Mul (Num a) x) (Mul y (Num b))) = simplify ((Num a) .* simplify x .- (Num b) .* simplify y)
simplify (Sub (Mul x (Num a)) (Mul (Num b) y)) = simplify ((Num a) .* simplify x .- (Num b) .* simplify y)
simplify (Sub (Mul x (Num a)) (Mul y (Num b))) = simplify ((Num a) .* simplify x .- (Num b) .* simplify y)
simplify (Sub (Neg e1) (Neg e2)) = simplify e1 .+ simplify e2
simplify (Sub (Neg e1) e2) = simplify (Neg e1) .- simplify e2
simplify (Sub e1 (Neg e2)) = simplify e1 .+ simplify e2
simplify (Sub x y) = if x == y then (Num 0) else (simplify x .- simplify y)

simplify (Mul (Num 1) e2) = simplify e2
simplify (Mul e1 (Num 1)) = simplify e1
simplify (Mul (Num 0) e2) = Num 0
simplify (Mul e1 (Num 0)) = Num 0
simplify (Mul (Num n) (Num m)) = Num $ n * m
simplify (Mul u (Num n)) = Num n .* simplify u
simplify (Mul (Neg u) (Neg v)) = simplify u .* simplify v
simplify (Mul (Neg u) v) = Neg (simplify u .* simplify v)
simplify (Mul u (Neg v)) = Neg (simplify u .* simplify v)
simplify (Mul e1 (Add x y)) = simplify e1 .* simplify x .+ simplify e1 .* simplify y
simplify m@(Mul (Mul (Num a) (Pow x (Num p)))
                (Mul (Num b) (Pow y (Num q))))
    = if x == y then (Num $ a * b) .* simplify x .^ (Num $ p + q) else simplify m

{- TODO is this necessary?
        simplify (Mul a (Mul b rest))
            | isNum a && isNum b = a .* b .* simplify rest
            | negNum a && negNum b = a .* b .* simplify rest
            | negNum a && isNum b = Neg a .* b .* simplify rest
            | isNum a && negNum b = Neg a .* b .* simplify rest
            | otherwise = simplify a .* simplify b .* simplify rest
            where negNum x = isNegNum x || isNumNeg x


        -- TODO after poly stuff fix this so that we simplify functions (go to simpFuncs partition)


        simplify (Mul (Mul (Mul rest f@(F _)) g@(F _)) h@(F _)) = simplify rest .* simplifyFunctions (f .* g .* h)
        simplify (Mul (Mul rest f@(F _)) g@(F _)) = simplify rest .* simplifyFunctions (f .* g)


        -- TODO now these functions are ready to go into the function simplifier (sin * cos * tan)

simplify (Mul (F (Sin u)) (F (Cos v)))
            = if u == v then F (Tan u) else (F (Sin $ simplify u)) .* (F (Cos $ simplify v))
        simplify (Mul t1@(F (Tan u)) t2@(F (Tan v)))
            = if u == v then (F (Tan u)) .^ Num 2 else simplify t1 .* simplify t2
        simplify (Mul (F (Sin u)) (F (Cos v))) -}

simplify poly@(Mul (Num n) maybePow)
    | n < 0 && isMono poly = Neg $ Mul (Num (-1 * n)) maybePow
    | n > 0 && isMono poly = poly
    | n < 0 = Mul (Neg (Num (-1*n))) maybePow
    | otherwise = Mul (Num n) (simplify maybePow)
simplify (Mul other (Div a b)) = Div (other .* a) b
simplify (Mul x y) = if x == y then simplify x .^ (Num 2) else simplify x .* simplify y

simplify (Div (Num 0) (Num 0)) = error "0/0 not possible"
simplify (Div (Num n) (Num m)) = Num $ n `div` m
simplify (Div (Num 0) e2) = Num 0
simplify (Div (Neg e1) (Neg e2)) = simplify $ simplify e1 ./ simplify e2
simplify (Div (Neg e1) e2) = Neg (simplify e1 ./ simplify e2)
simplify (Div e1 (Neg e2)) = Neg (simplify e1 ./ simplify e2)
simplify (Div e1 (Num 0)) = error "divide by zero!"
simplify (Div e1 (Num 1)) = simplify e1
simplify d@(Div (Mul (Num a) (Pow x (Num p)))
                (Mul (Num b) (Pow y (Num q))))
    = if x == y then (Num $ a `div` b) .* simplify x .^ (Num $ p - q) else simplify d
simplify (Div x y) = if x == y then (Num 1) else simplify x ./ simplify y

simplify (Pow (Num n) (Num m)) = Num $ n ^ m
simplify (Pow (Neg e1) (Neg e2)) = Num 1 ./ ((simplify (Neg e1)) .^ (simplify e2))
simplify (Pow (Neg e1) (Num p)) = if (even p) then (simplify e1 .^ Num p)
    else ((simplify (Neg e1)) .^ Num p)
simplify (Pow e1 (Neg e2)) = Num 1 ./ (simplify e1 .^ simplify e2)
simplify (Pow e (Num 0)) = Num 1
simplify (Pow (Num 0) e) = Num 0
simplify (Pow (Num 1) e) = Num 1
simplify (Pow e (Num 1)) = simplify e --- TODO do power rules next
simplify (Pow x y) = simplify x .^ simplify y

-- simplify (Neg (Num n)) = Num (-n)
simplify (Neg (Neg e)) = simplify e
simplify (Neg a@(Add e1 e2)) = simplify (Neg e1) .- simplify e2
simplify (Neg s@(Sub e1 e2)) = simplify (Neg e1) .+ simplify e2
simplify (Neg (Mul (Num n) x)) = Num (-n) .* simplify x
simplify (Neg m@(Mul _ _)) = Neg $ simplify m
simplify (Neg e) = Neg $ simplify e



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


simplifyComplete :: Expr -> Expr
simplifyComplete expr
    | s == expr = expr
    | otherwise = simplifyComplete $ simplify s
    where s = simplify expr


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
testMM5' = (show $ chisel mm5') == "{((7)sin(x)cos(x)(8)) / (x^22)}"
testMM6' = (show $ chisel mm6') == "{((7)sin(x)(x^2)) / (x^22)}"

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

testDM1' = (show $ chisel dm1') == "{({(7x(8)) / ((x + 1)^22)}) / (sin(x)cos(x))}"
testDM2' = (show $ chisel dm2') == "{({(7x(8)) / ((x + 1)^22)})/sin(x)}"
testDM3' = (show $ chisel dm3') == "{({(8) / ((x + 1)^22)}) / (sin(x)cos(x))}"
testDM4' = (show $ chisel dm4') == "{({(1) / ((x + 1)^22)}) / (sin(x)cos(x))}"
testDM5' = (show $ chisel dm5') == "{({(1) / ((x + 1)^22)})/sin(x)}"

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
