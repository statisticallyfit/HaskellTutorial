module Expr where

import Test.QuickCheck -- (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad hiding (join)
import Control.Applicative
import Data.Char
import Numeric
import Data.Maybe
import Data.List
-- import Data.List.Extra (trim, wordsBy)


data Function
    = Sin Expr | Cos Expr | Tan Expr |
      Csc Expr | Sec Expr | Cot Expr |
      Arcsin Expr | Arccos Expr | Arctan Expr |
      Arccsc Expr | Arcsec Expr | Arccot Expr |
      Sinh Expr | Cosh Expr | Tanh Expr |
      Csch Expr | Sech Expr | Coth Expr |
      Arcsinh Expr | Arccosh Expr | Arctanh Expr |
      Arccsch Expr | Arcsech Expr | Arccoth Expr |
      Ln Expr | E Expr | Log Expr Expr -- first expr is base
    deriving (Eq)


data Op = AddOp | SubOp | MulOp | DivOp | PowOp deriving (Eq, Show)

data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
    | Pow Expr Expr | Neg Expr | Num Int  {-Var Expr-} | X | Y | F Function
    deriving (Eq)

type Coeff = Int
type Description = (Expr, Expr, Expr)

data Group = Poly [Coeff] | Trig [Description] | InvTrig [Description] | Hyperbolic [Description] |
    InvHyp [Description] | Logarithmic [Description]
    deriving (Eq, Show)

data Code = Code [Group] deriving (Eq, Show)

data Tree a = Empty | Leaf a | Node String (Tree a) (Tree a) deriving (Eq)

-- TODO for fractional int dividing
-- a = fst . head $ readFloat "0.75" :: Rational




-- TODO update for division.
-- idea: if has Add or sub then we split and apply this to the elements.
-- precondition: argument is either power, num, neg, function.
-- RULE: if the first thing in the unglued list is a number then do not surround it with brackets.


-- note laces certain expressions with brackets.
lace :: String -> Expr -> String
lace acc e
    | isNeg e = if (acc == "" && (isAdd e || isSub e))
    then ("-(" ++ show (getNeg e) ++ ")") else if (not (acc == "") && (isAdd e || isSub e))
    then (acc ++ "( -(" ++ show (getNeg e) ++ "))") else if (not (acc == ""))
    then (acc ++ "( -(" ++ show (getNeg e) ++ "))") else ("-" ++ show (getNeg e))
    | acc == "" && isNum e = show e
    | isNum e = acc ++ "(" ++ show e ++ ")"
    | isMono e = acc ++ (concatMap show (split MulOp e))
    | isPow e = acc ++ "(" ++ show e ++ ")"
    | isFunction e || isMul e = acc ++ show e
    | isDiv e = decideDiv acc e
    | otherwise = acc ++ show e

decideDiv acc expr
    | (numTerms up > 1) && (numTerms lo > 1) = acc ++ " (" ++ show up ++ ") / (" ++ show lo ++ ")"
    | numTerms up > 1 = acc ++ " (" ++ show up ++ ") / " ++ show lo
    | numTerms lo > 1 = acc ++ show up ++ "/(" ++ show lo ++ ")"
    | otherwise = acc ++ show up ++ "/" ++ show lo
    where up = getUpper expr
          lo = getLower expr


-- | (isMono e) && (numTerms e <= 2) = putStrLn $ show $ simplifyComplete e -- is monomial then print simply.
-- TODO edit for division.
{-
printExpr :: Expr -> IO()
printExpr e
    | hasAdd e || hasSub e = putStrLn (foldl1 plus elementsStr)
    | otherwise = putStrLn (foldPrint e)
    where
    foldPrint e = foldl lace "" (split MulOp e)
    addSubSplit = map (split SubOp) (split AddOp e)
    elementsStr = map foldPrint addSubSplit
    plus acc x = if (head x == '-') then (acc ++ " - " ++ (tail x)) else (acc ++ " + " ++ x)
-}





{-

lace :: String -> Expr -> String
lace acc e
    | isNum e = if (acc == "") then show e else (acc ++ "(" ++ show e ++ ")")
    | isNegNum e || isNeg e = if (acc == "") then ("-" ++ show ne) else (acc ++ "(" ++ show e ++ ")")
    | isPow e = acc ++ "(" ++ show e ++ ")"
    | isDiv e = --peseudo code: acc ++ ( ++ printExpr e1 ++ ") / (" ++ print Expr e2 ++ ")" of div expr
    | isFunction e = acc ++ show e
    | otherwise = acc ++ "(" ++ show e ++ ")"
    where ne = getNeg e
-}






-- TODO idea: count num elements and then decide whether ot put brackets.
-- Example: x^6 * 4 is shown as 4x^6 while 4 * (x+3) is shown as 4(x+3)
-- idea: glued things are wrapped each.
-- NOTE original
instance Show Expr where
    show X = "x"
    show Y = "y"
    show (Num n) = show n
    show (F func) = show func
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2) = show e1 ++ " - " ++ show e2

    show (Mul (Num n) (Num m)) = "(" ++ show n ++ ")(" ++ show m ++ ")"
    show (Mul (Num n) p@(Pow _ _)) = show n ++ show p
    show (Mul (Num n) X) = show n ++ show X
    show (Mul (Neg (Num n)) X) = "-" ++ show n ++ show X
    --show (Neg (Mul (Num n) X)) = "-" ++ show n ++ show X
    show (Mul (Num n) Y) = show n ++ show Y
    show (Mul (Neg (Num n)) Y) = "-" ++ show n ++ show Y
    --show (Neg (Mul (Num n) Y)) = "-" ++ show n ++ show Y
    show (Mul (Num n) rest) = show n ++ "(" ++ show rest ++ ")"

    show (Mul rest p@(Pow _ _))
        = if isPow rest
        then "(" ++ show rest ++ ")(" ++ show p ++ ")"
        else show rest ++ "(" ++ show p ++ ")"

    show (Mul rest (Num n))
        = if isNum rest
        then "(" ++ show rest ++ ")(" ++ show n ++ ")"
        else show rest ++ "(" ++ show n ++ ")"

    show (Mul d1@(Div _ _) d2@(Div _ _)) = "(" ++ show d1 ++ ") (" ++ show d2 ++ ")"
    show (Mul d@(Div _ _) m@(Mul _ _)) = "(" ++ show d ++ ") (" ++ show m ++ ")"
    show (Mul m@(Mul _ _) d@(Div _ _)) = "(" ++ show m ++ ") (" ++ show d ++ ")"
    show (Mul m1@(Mul (Num a) (Pow _ _)) m2@(Mul (Num b) (Pow _ _)))
        = "(" ++ show m1 ++ ") (" ++ show m2 ++ ")"
    show (Mul e1 a@(Add _ _)) = show e1 ++ "(" ++ show a ++ ")"
    show (Mul e1 ng@(Neg (Num n))) = show e1 ++ "(" ++ show ng ++ ")"
    show (Mul e1 e2)
        = if isPow e1 && isPow e2
         then show e1 ++ " * " ++ show e2
         else if isPow e1 && isNum e2
         then "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"
         else show e1 ++ show e2

    show (Div (Num n) (Num m)) = show n ++ "/" ++ show m
    show (Div e1 e2) = "(" ++ show e1 ++ ") / (" ++ show e2 ++ ")"
    show (Pow x d@(Div (Num a) (Num b))) = show x ++ "^(" ++ show d ++ ")"
    show (Pow x d@(Div e1 e2)) = show x ++ "^" ++ show d
    show (Pow e1 e2) = show e1 ++ "^" ++ show e2

    show (Neg m@(Mul _ _)) = "-" ++ show m
    show (Neg d@(Div _ _)) = "-" ++ show d
    show (Neg p@(Pow _ _)) = "-" ++ show p
    show (Neg f@(F _)) = "-" ++ show f
    show (Neg X) = "-" ++ show X
    show (Neg Y) = "-" ++ show Y
    show (Neg e) = "-(" ++ show e ++ ")"


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


instance Show a => Show (Tree a) where
    show tree = draw 1 "\n" tree
        where
        draw _ _ Empty = "Empty"
        draw _ _ (Leaf n) = "Leaf " ++ show n
        draw count indent (Node n left right)
            = "Node " ++ (show n) ++ indent' ++ draw count' indent' left
            ++ indent' ++ draw count' indent' right
            where
            indent' = indent ++ "    "
            count' = count + 1


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
e1 = Num(4) .* X .* (F (Sin X)) .* Num(2) .* (F (Cos X)) .* Num(5) .* Num(2) .* Num(3) .* (F (Tan X))
e2 = e1 .+ Num(2) .* X .+ Num(7) .* X
e3 = Num(4) .* X .+ Num(3) .* X .^ Num(5) .- (F (Sin (Num(3) .+ X)))
e4 = Neg(F (Sin X)) .* Neg(Num(2))
e5 = Neg (Num(4) .* X .+ Num(2) .* Y)
e6 = Num (-7) .* X .^ Num 2 .+ Num 3 .* X .+ Num 4 .* X .+ Num 5 .* X .^ Num 2 .-
    Num 3 .* (F $ Sin $ Num 4 .* X) .+ Num 5 .* (F $ Cos X) .+ Num 2 .+ Num 3
e7 = (Num 3 .* X .^ Num 3) ./ (Num 3 .* X .^ (Num 1 ./ Num 3)) .-
    (Num 8 .* X .^ Num 9) ./ (Num 4 .* X .^ Num 3)
e8 = e6 .+ e7
-- NOTE IMPORTANT you must put (e1 * e2) / (e3 * e4) brakcets like that because otherwise
-- error says you cannot mix ./ and .* because one is infix l and other is infix r.
e9 = ((Num 3 .* X .^ Num 3) ./ (Num 3 .* X .^ (Num 1 ./ Num 3))) .*
     ((Num 8 .* X .^ Num 9) ./ (Num 4 .* X .^ Num 3))
e10 = (Num 3 .* X .^ Num 3) ./ ((Num 3 .* X .^ (Num 1 ./ Num 3)) .*
     (Num 8 .* X .^ Num 9)) ./ (Num 4 .* X .^ Num 3)
e11 = Num 4 .* (X .+ Num 3)
e12 = Num 2 .* X .^ Num 2 .* (Num 3 .* X .^ Num 5) .* (F (Sin (Num 4 .* X)))
    .* (F (Cos (Num 5 .* X .^ Num 2))) .^ Num 2 .* (Num 3 .* X .^ Num 9) .* (Num 2 ./ (F (Sin X)))
    ./ (Num 4 .* X .* (F (Sec X)) .* (F (Tan X)) .* Num 2)


-- TODO test percolate minus cases
t1 :: Tree Expr
t1 = Node "-" (Node "-" (Leaf $ Num 1) (Leaf $ Num 4)) (Leaf $ Num (-10))
t2 = Node "-" Empty (Node "*" (Leaf X) (Leaf $ Num 3))
t3 = Node "-" (Leaf $ Num 4) Empty


{-
NOTE:
TODO be able to fold over tree and simplify constants but also remember order that they
were in the tree!
Then remember also where they go at the end!
-- TODO get infinite decimal to fraction converter.
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
getUpper _ = error "not div expr in getUpper"

getLower :: Expr -> Expr
getLower (Div _ denom) = denom
getLower _ = error "not div expr in getLower"

isPow :: Expr -> Bool
isPow (Pow _ _) = True
isPow _ = False

isNum :: Expr -> Bool
isNum (Num _) = True
isNum _ = False

isNegNum :: Expr -> Bool
isNegNum (Neg (Num n)) = True
isNegNum _ = False

isVar :: Expr -> Bool
isVar X = True
isVar Y = True
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


---------------------------------
-- does not refer to data Op, just data Expr (Add, sub, mul..)
isOp :: Expr -> Bool
isOp e = isAdd e || isSub e || isMul e || isDiv e || isPow e


hasAdd :: Expr -> Bool
hasAdd X = False
hasAdd Y = False
hasAdd (Add e1 e2) = True
hasAdd (Num _) = False
hasAdd (Neg e) = hasAdd e
hasAdd (F f) = False
hasAdd (Sub e1 e2) = hasAdd e1 || hasAdd e2
hasAdd (Mul e1 e2) = hasAdd e1 || hasAdd e2
hasAdd (Div e1 e2) = hasAdd e1 || hasAdd e2
hasAdd (Pow e1 e2) = hasAdd e1 || hasAdd e2

hasSub :: Expr -> Bool
hasSub X = False
hasSub Y = False
hasSub (Sub e1 e2) = True
hasSub (Num _) = False
hasSub (Neg e) = hasSub e
hasSub (F f) = False
hasSub (Add e1 e2) = hasSub e1 || hasSub e2
hasSub (Mul e1 e2) = hasSub e1 || hasSub e2
hasSub (Div e1 e2) = hasSub e1 || hasSub e2
hasSub (Pow e1 e2) = hasSub e1 || hasSub e2

hasDiv :: Expr -> Bool
hasDiv X = False
hasDiv Y = False
hasDiv (Div e1 e2) = True
hasDiv (Num _) = False
hasDiv (Neg e) = hasDiv e
hasDiv (F f) = False
hasDiv (Add e1 e2) = hasDiv e1 || hasDiv e2
hasDiv (Sub e1 e2) = hasDiv e1 || hasDiv e2
hasDiv (Mul e1 e2) = hasDiv e1 || hasDiv e2
hasDiv (Pow e1 e2) = hasDiv e1 || hasDiv e2

hasFunction :: Expr -> Bool
hasFunction X = False
hasFunction Y = False
hasFunction (F _) = True
hasFunction (Num _) = False
hasFunction (Neg e) = hasDiv e
hasFunction (Add e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Sub e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Mul e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Div e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Pow e1 e2) = hasFunction e1 || hasFunction e2



-- counts total number of separate terms, where powers are counted as 1 term.
numTerms :: Expr -> Int
numTerms expr = length $ divid
    where
    added = split AddOp expr
    subbed = concatMap (split SubOp) added
    multip = concatMap (split MulOp) subbed
    divid = concatMap (split DivOp) multip

-- TODO rename split to take constructor and split based on that so split Mul and split Add and
-- split Sub and split DIv ...
-- TODO idea have list of lists to have divisions where division is inner but then
-- how far would it go as we want to have muliplications in the divisions too... HELP
-- postcondition takes an expression that just has * or / (no + or -) and  and returns the
-- terms in list order
{-unGlue :: Expr -> [Expr]
unGlue X = [X]
unGlue Y = [Y]
unGlue (Num n) = [Num n]
unGlue (Neg e) = [Neg (head (unGlue e))] ++ tail (unGlue e)
unGlue (F f) = [F f]
unGlue (Mul e1 e2) = unGlue e1 ++ unGlue e2
unGlue (Div e1 e2) = unGlue e1 ++ unGlue e2
unGlue p@(Pow _ _) = [p]-}

{-

isGlued :: Expr -> Bool
isGlued (Neg e) = categ e
isGlued e = categ e
    where
    categ x = isDiv x || isMul x || isPow x || isNum x || isFunction x || x == X || x == Y
-}

isSeparable :: Expr -> Bool
isSeparable (Neg e) =isAdd e || isSub e
isSeparable e = isAdd e || isSub e


-- precondition: the items in the list were multiplied only! Not divided or added or subtracted
-- TODO help improve (look at unglue todo)
glue :: [Expr] -> Expr
glue es = foldl1 Mul es



-- TODO apply the first mul case thinking to other cases to get all cases.
-- splits the terms in the expression at + or -
-- TODO rename unGlue to be splitGlue and the rebuild for glue to be rebuildGlue
split :: Op -> Expr -> [Expr]
split _ X = [X]
split _ Y = [Y]
split _ (Num n) = [Num n]
split _ (F f) = [F f]
split op (Neg e) = genSplit op (Neg e)
split op expr = pickMethod op expr

splitA :: Expr -> [Expr]
splitA e@(Add _ _) = splitAdd e
splitA (Sub e1 e2) = splitA e1 ++ splitA (Neg e2)
splitA (Mul e1 e2) = splitA e1 ++ splitA e2
splitA (Div e1 e2) = splitA e1 ++ splitA e2
splitA e = genSplit AddOp e

splitAdd (Add e1 e2)
    | notAdd e1 && notAdd e2 = [e1, e2]
    | notAdd e1 = [e1] ++ splitA e2
    | notAdd e2 = splitA e1 ++ [e2]
    | otherwise = splitA e1 ++ splitA e2
    where notAdd = not . isAdd


splitS :: Expr -> [Expr]
splitS (Add e1 e2) = splitS e1 ++ splitS e2
splitS e@(Sub _ _) = splitSub e
splitS (Mul e1 e2) = splitS e1 ++ splitS e2
splitS (Div e1 e2) = splitS e1 ++ splitS e2
splitS e = genSplit SubOp e

splitSub (Sub e1 e2)
    | notSub e1 && notSub e2 = [e1, Neg e2]
    | notSub e1 = [e1] ++ splitS (Neg e2)
    | notSub e2 = splitS e1 ++ [Neg e2]
    | otherwise = splitS e1 ++ map Neg (splitS e2)
    where notSub = not . isSub


splitM :: Expr -> [Expr]
splitM (Add e1 e2) = splitM e1 ++ splitM e2
splitM (Sub e1 e2) = splitM e1 ++ splitM (Neg e2)
splitM e@(Mul _ _) = splitMul e
splitM (Div e1 e2) = splitM e1 ++ splitM e2
splitM e = genSplit MulOp e

splitMul (Mul e1 e2)
    | notMul e1 && notMul e2 = [e1, e2]
    | notMul e1 = [e1] ++ splitM e2
    | notMul e2 = splitM e1 ++ [e2]
    | otherwise = splitM e1 ++ splitM e2
    where notMul = not . isMul


splitD :: Expr -> [Expr]
splitD (Add e1 e2) = splitD e1 ++ splitD e2
splitD (Sub e1 e2) = splitD e1 ++ splitD (Neg e2)
splitD (Mul e1 e2) = splitD e1 ++ splitD e2
splitD e@(Div _ _) = splitDiv e
splitD e = genSplit DivOp e

splitDiv (Div e1 e2)
    | notDiv e1 && notDiv e2 = [e1, e2]
    | notDiv e1 = [e1] ++ splitD e2
    | notDiv e2 = splitD e1 ++ [e2]
    | otherwise = splitD e1 ++ splitD e2
    where notDiv = not . isDiv



pickMethod :: Op -> Expr -> [Expr]
pickMethod AddOp expr = splitA expr
pickMethod SubOp expr = splitS expr
pickMethod MulOp expr = splitM expr
pickMethod DivOp expr = splitD expr
pickMethod _ _  = error "only ops are: add, sub, mul, div"



genSplit :: Op -> Expr -> [Expr]
genSplit op X = [X]
genSplit op Y = [Y]
genSplit op (Num n) = [Num n]
genSplit op (F f) = [F f]
genSplit op (Neg e)  -- = if isGlued e then [Neg e] else (map Neg ((pickMethod op) e))
    | isAdd e || isSub e = map Neg pick
    | isMul e || isDiv e || isPow e = [Neg (head pick)] ++ tail pick
    | otherwise = [Neg (head gen)] ++ tail gen
    where pick = pickMethod op e
          gen = genSplit op e

{-

unGlue X = [X]
unGlue Y = [Y]
unGlue (Num n) = [Num n]
unGlue (Neg e) = [Neg (head (unGlue e))] ++ tail (unGlue e)
unGlue (F f) = [F f]
unGlue (Mul e1 e2) = unGlue e1 ++ unGlue e2
unGlue (Div e1 e2) = unGlue e1 ++ unGlue e2
unGlue p@(Pow _ _) = [p]
-}



-- rebuilds with add sub signs.
rebuildAS :: [Expr] -> Expr
rebuildAS es = foldl1 f es
    where f acc x = if isNeg x then (Sub acc (getNeg x)) else (Add acc x)

rebuildA :: [Expr] -> Expr
rebuildA es = foldl1 (\acc x -> Add acc x) es

rebuildS :: [Expr] -> Expr
rebuildS es = foldl1 (\acc x -> Sub acc x) es

-- rebuilds with Mul op
rebuildM :: [Expr] -> Expr
rebuildM es = foldl1 (\acc x -> Mul acc x) es

rebuildD :: [Expr] -> Expr
rebuildD es = foldl1 (\acc x -> Div acc x) es



{-
TODO PLAN OVERALL * ~ *

input -> split Add and Sub -> partition -> (poly terms (1), other)

other -> if isMul then split Mul and if Div each then split DIv each
    -> partition -> (poly terms (2), functions)

    (same for div top and bottom)

functions -> send to function simplifyFunctions (or fold using simplifyFunctions principles)
principle:
represent sin ^ tan x (x) or sin^ 2 (3x^6) as:
Trig [(tan x, X), 0,0,0,0,0] and,
Trig [(Num 2, (3x^6)),0,0,0,0,0] respectively.

principle: we add only if both elements in tuple are equal pairwise, (same for sub) resulting in
new data holder: Trig [(Num 2, same exp, same arg)]

and we multiply only if the argument is equal (snd) so we get Trig [(tanx + Num 2 as exp, X)]
assuming arg was X for both. same for div

---
poly terms (1) -> (make sure to split by both add and sub) then send to addH

poly terms (2) -> these are split by Mul, so if any of these elements "have Div" then split that one
by Div and send to divH. Otherwise, send to mulH.

---
Clean up with sweep Num _
-}


-- note takes a codeified polynomial, or trig or invtrig... and adds the two codified things.
-- note todo currently just impleneted for codified polynomial
--addH :: [(Coeff, Expo, Expr)] -> [(Coeff, Expo, Expr)] -> Expr
-- addCodes :: Code -> Code -> Code
addCodes h1 h2 = simplifiedMaybes
    where
    h1' = map numify h1
    h2' = map numify h2
    add (a@(c1,e1,x1),b@(c2,e2,x2)) =
        if x1 == x2 then (Just (c1 .+ c2,e1,x1), Nothing)
        else (Just a, Just b)
    simpFirst (x,y,z) = (simplify x, y, z)
    simpMaybe (a,b) =
        if (isJust a && isNothing b) then (Just $ simpFirst $ fromJust a, Nothing)
        else (a,b)
    simplifiedMaybes = map simpMaybe $ map add (zip h1' h2')

list = map numify [(4,1,X), (3,1,X), (1,7,(Num 2) .* X .^ Num 5), (2,2,X),(1,1,X), (7,7,Num 7 .* X)]
add (a@(c1,e1,x1),b@(c2,e2,x2)) = if x1 == x2 then (Just (c1 .+ c2,e1,x1), Nothing) else (Just a, Just b)
hs = zip list list
js = map add hs
clean' (x,y,z) = (simplify x, y, z)
ms = map (\(a, b) -> if (isJust a && isNothing b) then (Just $ clean' $ fromJust a, Nothing) else (a,b)) js
--g (a@(c1,e1,x1), b@(c2,e2,x2)) = if (x1 == x2) then (c1 .+ c2,e1,x1) else (a,b)



-- note says if expression is a single polynomial term like 5x (monomial)
isMono :: Expr -> Bool
isMono e
    | hasAdd e || hasSub e || hasDiv e || hasFunction e = False
    | otherwise = foldl f True s'
    where
    e' = simplifyComplete e
    s' = map simplifyComplete $ split MulOp e'
    f = (\acc x -> acc && (isNum x || isVar x || isPolyPow x))
    isPolyPow (Pow X (Num n)) = True
    isPolyPow (Pow Y (Num n)) = True
    isPolyPow _ = False


-- note says if expression contains no functions and is just a polynomial term like 7x^2 / 6x or just 5x
isPoly :: Expr -> Bool
--isPoly = not . isFunction
isPoly X = True
isPoly Y = True
isPoly (Num n) = True
isPoly (F f) = False
isPoly (Neg e) = isPoly e
isPoly (Add e1 e2) = isPoly e1 && isPoly e2
isPoly (Sub e1 e2) = isPoly e1 && isPoly e2
isPoly (Mul e1 e2) = isPoly e1 && isPoly e2
isPoly (Div e1 e2) = isPoly e1 && isPoly e2
isPoly (Pow e1 e2) = isPoly e1 && isPoly e2

isTrig :: Expr -> Bool
isTrig f = isSin f || isCos f || isTan f || isCsc f || isSec f || isCot f
{-

-- note this is passed only glued expressions!
-- example hasOnlyOne isTrig 3x^7x^2sin(4x) = True
hasOnlyOne :: (Expr -> Bool) -> Expr -> Bool
hasOnlyOne f expr
    | isGlued expr = (length $ filter (== True) $ map f (unGlue expr)) == 1
    | otherwise = error "was passed non-glued expression"
-}


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

-- note TODO look at plan above split keep this for reference if above doesn't work but it seems to ..

-- note converts expression to code
-- first splits expr at the plus / minus signs and then categorizes each element as either
-- function or polynomial term. Puts them into code accordingly.
-- If we have 7x^2 and 3x^2 in the same expr, then we have: Poly [0,0,(7+3)
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
exprToCode :: Expr -> Code
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
    emptyGroup (Logarithmic ls) = null ls
-}
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




-- note takes list of polynomials and makes it into Group type Poly [...]
-- so 7x^2 + 3x^2 + 3x + 4x + 1 is [1, (3+4), (7+3)]
codifyPoly :: [Expr] -> Group
codifyPoly [] = Poly []
codifyPoly ps = Poly $ foldl1 (zipWith (+)) $ addZeroes $ codify ps
    where -- note poly expects no forms like 7x / 4x^3 in the list element position
    addZeroes cs = map (\xs -> xs ++ replicate (maxCodeLen - length xs) 0) cs
    codify ps = map poly (map simplify ps)
    maxCodeLen = foldl1 max $ map length (codify ps)
    poly X = [0, 1]
    poly Y = [0, 1]
    poly (Num n) = [n]
    poly (F func) = error "no functions allowed in makePoly"
    poly (Neg e) = poly e
    poly (Mul (Neg (Num n)) (Pow X (Num p))) = replicate p 0 ++ [-n]
    poly (Mul (Neg (Num n)) X) = [0, -n]
    poly (Mul (Num n) (Pow X (Num p))) = replicate p 0 ++ [n]
    poly (Mul (Num n) X) = [0, n]





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


-- TODO types wrong
--- insdie the function is a type expression holder that is converted to expression.
{-holderExprFunctions :: Expr -> Expr
holderExprFunctions (F (Sin h)) = -}


-- puts an expression inside a function
push :: Expr -> Expr -> Expr
push u (F (Sin v)) = F $ Sin u
push u (F (Cos v)) = F $ Cos u
push u (F (Tan v)) = F $ Tan u
push u (F (Csc v)) = F $ Csc u
push u (F (Sec v)) = F $ Sec u
push u (F (Cot v)) = F $ Cot u
push u (F (Arcsin v)) = F $ Arcsin u
push u (F (Arccos v)) = F $ Arccos u
push u (F (Arctan v)) = F $ Arctan u
push u (F (Arccsc v)) = F $ Arccsc u
push u (F (Arcsec v)) = F $ Arcsec u
push u (F (Arccot v)) = F $ Arccot u
push u (F (Sinh v)) = F $ Sinh u
push u (F (Cosh v)) = F $ Cosh u
push u (F (Tanh v)) = F $ Tanh u
push u (F (Csch v)) = F $ Csch u
push u (F (Sech v)) = F $ Sech u
push u (F (Coth v)) = F $ Coth u
push u (F (Arcsinh v)) = F $ Arcsinh u
push u (F (Arccosh v)) = F $ Arccosh u
push u (F (Arctanh v)) = F $ Arctanh u
push u (F (Arccsch v)) = F $ Arccsch u
push u (F (Arcsech v)) = F $ Arcsech u
push u (F (Arccoth v)) = F $ Arccoth u
push u (F (E v)) = F $ E u
push u (F (Ln v)) = F $ Ln u
push u (F (Log v1 v2)) = F $ Log u u




-- TODO major help why doesn't 4(x+3) simplify to 4x + 12, why 4x + (4)(3)?? ?
-- TODO probably because this has gotten too old - perhaps ther eis a case that goes ahead of the
-- one that should be entered that keeps it in this ugly state? Fix with printExpr and foldl.
simplify :: Expr -> Expr
simplify X = X
simplify Y = Y
simplify (Num n) = Num n
simplify (F (Sin e)) = F $ Sin $ simplify e
simplify (F (Cos e)) = F $ Cos $ simplify e
simplify (F (Tan e)) = F $ Tan $ simplify e
simplify (F (Csc e)) = F $ Csc $ simplify e
simplify (F (Sec e)) = F $ Sec $ simplify e
simplify (F (Cot e)) = F $ Cot $ simplify e
simplify (F (Arcsin e)) = F $ Arcsin $ simplify e
simplify (F (Arccos e)) = F $ Arccos $ simplify e
simplify (F (Arctan e)) = F $ Arctan $ simplify e
simplify (F (Arccsc e)) = F $ Arccsc $ simplify e
simplify (F (Arcsec e)) = F $ Arcsec $ simplify e
simplify (F (Arccot e)) = F $ Arccot $ simplify e
simplify (F (Sinh e)) = F $ Sinh $ simplify e
simplify (F (Cosh e)) = F $ Cosh $ simplify e
simplify (F (Tanh e)) = F $ Tanh $ simplify e
simplify (F (Csch e)) = F $ Csch $ simplify e
simplify (F (Sech e)) = F $ Sech $ simplify e
simplify (F (Coth e)) = F $ Coth $ simplify e
simplify (F (Arcsinh e)) = F $ Arcsinh $ simplify e
simplify (F (Arccosh e)) = F $ Arccosh $ simplify e
simplify (F (Arctanh e)) = F $ Arctanh $ simplify e
simplify (F (Arccsch e)) = F $ Arccsch $ simplify e
simplify (F (Arcsech e)) = F $ Arcsech $ simplify e
simplify (F (Arccoth e)) = F $ Arccoth $ simplify e
simplify (F (E e)) = F $ E $ simplify e
simplify (F (Ln e)) = F $ Ln $ simplify e
simplify (F (Log e1 e2)) = F $ Log (simplify e1) (simplify e2)

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
simplify (Mul a (Mul b rest))
    | isNum a && isNum b = a .* b .* simplify rest
    | isNegNum a && isNegNum b = a .* b .* simplify rest
    | isNegNum a && isNum b = Neg a .* b .* simplify rest
    | isNum a && isNegNum b = Neg a .* b .* simplify rest
    | otherwise = simplify a .* simplify b .* simplify rest
-- TODO after poly stuff fix this so that we simplify functions (go to simpFuncs partition)
{-
simplify (Mul (Mul (Mul rest f@(F _)) g@(F _)) h@(F _)) = simplify rest .* simplifyFunctions (f .* g .* h)
simplify (Mul (Mul rest f@(F _)) g@(F _)) = simplify rest .* simplifyFunctions (f .* g)
-}
-- TODO now these functions are ready to go into the function simplifier (sin * cos * tan)
{-simplify (Mul (F (Sin u)) (F (Cos v)))
    = if u == v then F (Tan u) else (F (Sin $ simplify u)) .* (F (Cos $ simplify v))
simplify (Mul t1@(F (Tan u)) t2@(F (Tan v)))
    = if u == v then (F (Tan u)) .^ Num 2 else simplify t1 .* simplify t2
simplify (Mul (F (Sin u)) (F (Cos v)))-}
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

simplify (Neg (Num n)) = Num (-n)
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
sweep :: (Expr -> Bool) -> Expr -> Expr
sweep f expr = glue $ [itemSwept] ++ remainder
    where
    elements = split MulOp expr
    itemSwept = simplifyComplete $ foldl1 (\acc y -> if f y then (acc .* y) else acc) elements
    remainder = filter (not . f) elements
    -- TODO e4 breaks at foldl1 because 2 is ignored.. FIX.


simplifyComplete :: Expr -> Expr
simplifyComplete expr
    | s == expr = expr
    | otherwise = simplifyComplete $ simplify s
    where s = simplify expr


sameArgs :: Function -> Function -> Bool
sameArgs f g
    | getArg f == getArg g = True
    | otherwise = False

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




-- TODO also make another operator that is true for (3)(X) == (X)(3)
-- and 3 + x == x + 3

-- TODO make helper function to count number of "/" so we know whether to divide or multiply
-- the constants. So if odd "/" then divide else multiply.


-- TODO fix percolation so that we don't get order of operations wrong.

leftSub :: Tree a -> Tree a
leftSub (Node _ left _) = left
leftSub _ = error "leftSub"

rightSub :: Tree a -> Tree a
rightSub (Node _ _ right) = right
rightSub _ = error "rightSub"

isNode :: Tree a -> Bool
isNode (Node _ _ _) = True
isNode _ = False

isLeaf :: Tree a -> Bool
isLeaf (Leaf x) = True
isLeaf _ = False

leafVal :: Tree a -> a
leafVal (Leaf x) = x
leafVal _ = error "leafVal"

treeOp :: Tree a -> String
treeOp (Node op _ _) = op
treeOp _ = error "treeOp"

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

isThree :: Tree a -> Bool
isThree (Node n (Leaf x) (Leaf y)) = True
isThree _ = False


depth :: Tree a -> Int
depth Empty = 0
depth (Leaf n) = 1
depth (Node n left right) = 1 + max (depth left) (depth right)


mapTree :: (Int -> Int) -> Tree Expr -> Tree Expr
mapTree _ Empty = Empty
mapTree f (Leaf (Num n)) = Leaf (Num (f n))
mapTree f leaf@(Leaf l) = leaf -- TODO extend tree so that if f = +1 ans sinx then 1 (Node +) sin x
mapTree f (Node n left right) = Node n (mapTree f left) (mapTree f right)



mkTree :: Expr -> Tree Expr
mkTree X = Leaf X
mkTree Y = Leaf Y
mkTree (Num n) = Leaf (Num n)
mkTree (F func) = Leaf (F func) -- TODO should I show expressions in functino as a tree as well?
mkTree (Neg (Num n)) = Leaf (Num (-n))
mkTree (Neg e) = Node "-" Empty (mkTree e)
mkTree (Add e1 e2) = Node "+" (mkTree e1) (mkTree e2)
mkTree (Sub e1 e2) = Node "-" (mkTree e1) (mkTree e2)
mkTree (Mul e1 e2) = Node "*" (mkTree e1) (mkTree e2)
mkTree (Div e1 e2) = Node "/" (mkTree e1) (mkTree e2)
mkTree (Pow e1 e2) = Node "^" (mkTree e1) (mkTree e2)


--- testing that mkTree and expr are inverses of each other.
getExpr :: Tree Expr -> Expr
getExpr (Leaf x) = x
getExpr (Node "-" Empty right) = Neg (getExpr right)
getExpr (Node "+" left right) = Add (getExpr left) (getExpr right)
getExpr (Node "-" left right) = Sub (getExpr left) (getExpr right)
getExpr (Node "*" left right) = Mul (getExpr left) (getExpr right)
getExpr (Node "/" left right) = Div (getExpr left) (getExpr right)
getExpr (Node "^" left right) = Pow (getExpr left) (getExpr right)



-- note: moves product/sum of constants to last level that a constant was present. Earlier constants
-- are replaced by Empty. Other node sor functions or variables are kept.
percolate :: Tree Expr -> Tree Expr
percolate (Node op (Leaf s) Empty) = Leaf s
percolate node@(Node op Empty (Leaf s)) = node
percolate (Node op Empty right) = Node op Empty (percolate right) -- might just be neg so we need struct
percolate (Node op left Empty) = percolate left
percolate (Node op left right)
    | op == "+" = perc (0 +) $ Node "+" left right
    | op == "-" = perc gSub $ Node "-" left right
    | op == "*" = perc (1 *) $ Node "*" left right
    | op == "/" = perc gDiv $ Node "/" left right
    | op == "^" = perc gPow $ Node "^" left right
    where gSub = \x -> x - 0
          gDiv = \x -> x `div` 1
          gPow = \x -> x ^ 1



-- assume no constants in the right subtree, just on the left, always, assume we have
-- already passed this tree into the rearrange const function
-- TODO need to fix this to avoid cases where op is lower precedence than function
-- example: op = + and we plus over leaf connected to a "*"
perc :: (Int -> Int) -> Tree Expr -> Tree Expr
perc f (Node op (Leaf (Num n)) (Leaf (Num m)))
    | op == "+" = Leaf (Num ((f n) + m))
    | op == "-" = Leaf (Num ((f n) - m))
    | op == "*" = Leaf (Num ((f n) * m))
    | op == "/" = Leaf (Num ((f n) `div` m))
    | op == "^" = Leaf (Num ((f n) ^ m))
perc f (Node op (Leaf (Num n)) leaf@(Leaf varOrFunc)) = Node op (Leaf (Num (f n))) leaf
perc f (Node op leaf@(Leaf varOrFunc) (Leaf (Num m))) = Node op leaf (Leaf (Num (f m)))
perc f (Node op v1@(Leaf varOrFunc1) v2@(Leaf varOrFunc2))
    | op == "+" = Leaf (Num (f 0)) --Node op (Node op Empty (Leaf (Num (f 0)))) (Node op v1 v2)
    | op == "-" = Leaf (Num (f 0))
    | op == "*" = Leaf (Num (f 1))
    | op == "/" = Leaf (Num (f 1))
    | op == "^" = Leaf (Num (f 1))
perc f (Node op (Leaf (Num n)) right) = Node op Empty (perc g right)
    where (g, _) = opFunc op f n
perc f (Node op left (Leaf (Num m))) = Node op (perc h left) Empty
    where (_, h) = opFunc op f m
perc f (Node op leaf@(Leaf varOrFunc) right) = Node op leaf (perc f right)
perc f (Node op left leaf@(Leaf varOrFunc)) = Node op (perc f left) leaf

perc f (Node op Empty right) = Node op Empty (perc f right)
perc f (Node op left Empty) = perc f left
-- note: the node-node cases
perc f (Node op left right) = Node op (perc f left) (perc f right)


opFunc :: String -> (Int -> Int) -> Int -> ((Int -> Int), (Int -> Int))
opFunc op f n
    | op == "+" = (((f n) +), (\x -> x + (f n)))
    | op == "-" = (((f n) -), (\x -> x - (f n)))
    | op == "*" = (((f n) *), (\x -> x * (f n)))
    | op == "/" = (((f n) `div`), (\x -> x `div` (f n)))
    | op == "^" = (((f n) ^), (\x -> x ^ (f n)))


-- makes tree shorter by removing given thing.
-- NOTE this is awesomely cool function ! Test thoroughly to make sure not missing any cases.
remove :: Tree Expr -> Tree Expr -> Tree Expr
remove t Empty = Empty
remove t (Leaf s) = if t == (Leaf s) then Empty else Leaf s
remove t node@(Node _ l1@(Leaf x) l2@(Leaf y))
    | l1 == t = l2
    | l2 == t = l1
    | otherwise = node
remove t node@(Node op l1@(Leaf x) right)
    | right == t = l1
    | otherwise = Node op l1 (remove t right)
remove t node@(Node op left l2@(Leaf y))
    | left == t = l2
    | otherwise = Node op (remove t left) l2
remove t node@(Node op left right)
    | t == left = remove t right
    | t == right = remove t left
    | otherwise = Node op (remove t left) (remove t right)


-- note findConst returns the first constant we find or else Nothing
-- precondition: has been passed to percolate first, so that the constant we get is the
-- last one in the tree.
findConst :: Tree Expr -> Maybe Int
findConst Empty = Nothing
findConst (Leaf (Num n)) = Just n
findConst (Leaf varOrFunc) = Nothing
findConst (Node op (Leaf (Num n)) (Leaf (Num m)))
    = error "findConst: not simplified! n, m on same branch"
findConst (Node op (Leaf (Num n)) leaf@(Leaf varOrFunc)) = Just n
findConst (Node op leaf@(Leaf varOrFunc) (Leaf (Num n)) ) = Just n
findConst (Node op (Leaf l1) (Leaf l2)) = Nothing
findConst (Node op (Leaf l) right) = findConst right
findConst (Node op left (Leaf l)) = findConst left
findConst (Node op left right) = error "help" -- TODO what to do?


-- note puts the constant given in the first available place (right under first node)
-- note if we have a leaf we must use the previous operator, not make one up!
crownTree :: Maybe String -> Int -> Tree Expr -> Tree Expr
crownTree opMaybe n tree
    | isNothing opMaybe = crown n tree
    | otherwise = Node (fromJust opMaybe) (Leaf (Num n)) tree
    where
    crown n Empty = Empty
    crown n (Leaf (Num m)) = Leaf (Num $ n + m) --- HELP todo
    crown n leaf@(Leaf varOrFunc) = Node "+" (Leaf (Num n)) leaf
    crown n tree@(Node op left right) = Node op (Leaf (Num n)) tree


-- TODO fix because percolate makes x^2 * 3 into 6^X
-- precondition: passed to percolation so there are no patterns like Node num num.
-- precondition: passed to remove Empty _ after first thing so no Empty trees.
-- precondition: expects splitted output so just glued expressions (mult, dived), never +, -
rearrangeConsts :: Tree Expr -> Tree Expr
rearrangeConsts Empty = Empty
rearrangeConsts (Leaf s) = Leaf s
rearrangeConsts tree
    | isLeaf tree' = tree'
    | isNothing constMaybe = Empty
    -- | isLeaf tree'' = crownTree opMaybe num tree''
    | otherwise = crownTree opMaybe num tree''
    where tree' = remove Empty $ percolate tree
          constMaybe = findConst tree'
          num = fromJust constMaybe
          (tree'', opMaybe) = if isThree tree'
                         then (remove (Leaf (Num num)) tree', Just $ treeOp tree')
                         else (remove (Leaf (Num num)) tree', Nothing)






---------------------------------------------------------------------------------------------------

instance Arbitrary Expr where
    arbitrary = sized arbExpr
arbExpr 0 = liftM Num arbitrary
arbExpr n = frequency [(2, return X), (2, return Y),
                       (1, liftM Num arbitrary),
                       (4, liftM Neg arbitrary),
                       (4, liftM2 Add (arbExpr (n `div` 2)) (arbExpr (n `div` 2))),
                       (4, liftM2 Sub (arbExpr (n `div` 2)) (arbExpr (n `div` 2))),
                       (4, liftM2 Mul (arbExpr (n `div` 2)) (arbExpr (n `div` 2))),
                       (4, liftM2 Div (arbExpr (n `div` 2)) (arbExpr (n `div` 2))),
                       (4, liftM2 Pow (arbExpr (n `div` 2)) (arbExpr (n `div` 2)))]


instance Arbitrary Function where
    arbitrary = sized arbFunc
arbFunc n = frequency [(4, liftM Sin arbitrary),
                       (4, liftM Cos arbitrary),
                       (4, liftM Tan arbitrary),
                       (4, liftM Csc arbitrary),
                       (4, liftM Sec arbitrary),
                       (4, liftM Cot arbitrary),
                       (4, liftM Arcsin arbitrary),
                       (4, liftM Arccos arbitrary),
                       (4, liftM Arctan arbitrary),
                       (4, liftM Arccsc arbitrary),
                       (4, liftM Arcsec arbitrary),
                       (4, liftM Arctan arbitrary)]


instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbTree
arbTree 0 = return Empty --liftM NilT arbitrary
arbTree n = frequency [(1, return Empty),
                       (3, liftM Leaf arbitrary),
                       (4, liftM3 Node arbitrary (arbTree (n `div` 2))
                                                 (arbTree (n `div` 2)) )]




{-

e3 should look like:
(-)
 |
 `-- (+)
 |    |
 |    `-- (*)
 |    |    |
 |    |    `-- 4
 |    |    `-- x
 |    |
 |    `-- (*)
 |         |
 |         `-- 3
 |         `-- (^)
 |              |
 |              `-- x
 |              `-- 5
 |
 `-- sin(*) ...

instance Show a => Show (Tree a) where
    show theTree = draw 1 "\n     " "\n" "\n" theTree
        where
        draw _ _ _ _ Empty = "_"
        draw _ _ _ _ (Leaf n) = show n
        draw c i lb rb (Node op left right)
            | isNode left && isNode right
                = "(" ++ op ++ ")" ++ lb' ++ draw c' i' lb' rb' left ++
                    lb' ++ draw c' i' lb' rb' right
            | isNode left
                = "(" ++ op ++ ")" ++ lb' ++ draw c' i' lb' rb' left ++
                    rb' ++ draw c' i' lb' rb' right
            | isNode right
                = rb' ++ draw c' i' lb' rb' left ++
                    "(" ++ op ++ ")" ++ lb' ++ draw c' i' lb' rb' right
            where
            i' = i ++ "     "
            c' = c + 1
            lb' = lb ++ " |\n `-- "
            rb' = " |\n"  ++ i'
-}
