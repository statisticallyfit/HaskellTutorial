module Rose where

{-import Test.QuickCheck -- (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes-}
import Control.Monad hiding (join)
import Control.Applicative
import Data.Char
import Numeric
import Data.Maybe
import Data.List
-- import Data.List.Extra (trim, wordsBy)

data FunctionL
    = SinL ExprList | CosL ExprList | TanL ExprList |
      CscL ExprList | SecL ExprList | CotL ExprList |
      ArcsinL ExprList | ArccosL ExprList | ArctanL ExprList |
      ArccscL ExprList | ArcsecL ExprList | ArccotL ExprList |
      SinhL ExprList | CoshL ExprList | TanhL ExprList |
      CschL ExprList | SechL ExprList | CothL ExprList |
      ArcsinhL ExprList | ArccoshL ExprList | ArctanhL ExprList |
      ArccschL ExprList | ArcsechL ExprList | ArccothL ExprList |
      LnL ExprList | EL ExprList | LogL ExprList ExprList -- first expr is base
    deriving (Eq, Show)


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


data ExprList = AddL [ExprList] | SubL [ExprList] | MulL [ExprList] | DivL [ExprList]
    | PowL [ExprList] [ExprList] | NegL ExprList | NumL Int | VarL String | G FunctionL
    deriving (Eq, Show)


data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Pow Expr Expr
    | Neg Expr | Num Int | Var String | F Function
    deriving (Eq)

xl = VarL "x"
yl = VarL "y"
x = Var "x"
y = Var "y"

type Coeff = Int
type Description = (Expr, Expr, Expr)

data Group = Poly [Coeff] | Trig [Description] | InvTrig [Description] | Hyperbolic [Description] |
    InvHyp [Description] | Logarithmic [Description]
    deriving (Eq, Show)

data Code = Code [Group] deriving (Eq, Show)

data Tree a = Empty | Leaf a | Node String (Tree a) (Tree a) deriving (Eq)

data RoseTree a = EmptyRose | Petal a | Briar Op [RoseTree a] deriving (Eq, Show)
-- TODO for fractional int dividing
-- a = fst . head $ readFloat "0.75" :: Rational


-- TODO idea: count num elements and then decide whether ot put brackets.
-- Example: x^6 * 4 is shown as 4x^6 while 4 * (x+3) is shown as 4(x+3)
-- idea: glued things are wrapped each.
-- NOTE original
instance Show Expr where
    show (Var x) = x
    show (Num n) = show n
    show (Neg (Num n)) = if (n < 0) then ("-(" ++ show n ++ ")") else ("-" ++ show n)
    show (F func) = show func
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2)
        | isSeparable e2 = show e1 ++ " - [" ++ show e2 ++ "]"
        | otherwise = show e1 ++ " - " ++ show e2

    show (Mul (Num n) (F f)) = show n ++ show f
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
    show (Div e1 e2)
        | few e1 && few e2 = surround $ show e1 ++ "/" ++ show e2
        | few e1 = surround $ show e1 ++ "/(" ++ show e2 ++ ")"
        | few e2 = surround $ "(" ++ show e1 ++ ")/" ++ show e2
        | otherwise = surround $ "(" ++ show e1 ++ ") / (" ++ show e2 ++ ")"
        where
        surround eStr = "{" ++ eStr ++ "}"
        few e = (isVar e || isNum e || (not $ isMono e))
            && ((not $ isNumNeg e) && (not $ isNegNum e)
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
e1 = Num(4) .* x .* (F (Sin x)) .* Num(2) .* (F (Cos x)) .* Num(5) .* Num(2) .* Num(3) .* (F (Tan x))
e2 = e1 .+ Num(2) .* x .+ Num(7) .* x
e3 = Num(4) .* x .+ Num(3) .* x .^ Num(5) .- (F (Sin (Num(3) .+ x)))
e4 = Neg(F (Sin x)) .* Neg(Num(2))
e5 = Neg (Num(4) .* x .+ Num(2) .* y)
e6 = Num (-7) .* x .^ Num 2 .+ Num 3 .* x .+ Num 4 .* x .+ Num 5 .* x .^ Num 2 .-
    Num 3 .* (F $ Sin $ Num 4 .* x) .+ Num 5 .* (F $ Cos x) .+ Num 2 .+ Num 3
e7 = (Num 3 .* x .^ Num 3) ./ (Num 3 .* x .^ (Num 1 ./ Num 3)) .-
    (Num 8 .* x .^ Num 9) ./ (Num 4 .* x .^ Num 3)
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



splitRTest = Num 4 .- Num 1 .+ Num 3 .+ Num 5 .- Num 6 .- Num 7 .- Num 8 .+ Num 9 .- Num 2

rose = Briar AddOp [Briar MulOp [Petal (Num 4), Petal x], Petal x, Petal y, Petal (Num 3),
    Petal (Num 2), Petal (Num 1), Petal (Num 8)]


r1 = Briar AddOp [Briar SubOp [EmptyRose, Briar MulOp [Petal (Num 7),
    Briar PowOp [Petal x, Petal (Num 2)]]], Petal x, Petal (Num 1), Petal y, Petal (Num 2)]

{-

convertFunction :: Function -> FunctionL
convertFunction (Sin u ) = SinL u
convertFunction (Cos u ) = CosL u
convertFunction (Tan u ) = TanL u
convertFunction (Csc u ) = CscL u
convertFunction (Sec u ) = SecL u
convertFunction (Cot u ) = CotL u
convertFunction (Arcsin u ) = ArcsinL u
convertFunction (Arccos u ) = ArccosL u
convertFunction (Arctan u ) = ArctanL u
convertFunction (Arccsc u ) = ArccscL u
convertFunction (Arcsec u ) = ArcsecL u
convertFunction (Arccot u ) = ArccotL u
convertFunction (Sinh u ) = SinhL u
convertFunction (Cosh u ) = CoshL u
convertFunction (Tanh u ) = TanhL u
convertFunction (Csch u ) = CschL u
convertFunction (Sech u ) = SechL u
convertFunction (Coth u ) = CothL u
convertFunction (Arcsinh u ) = ArcsinhL u
convertFunction (Arccosh u ) = ArccoshL u
convertFunction (Arctanh u ) = ArctanhL u
convertFunction (Arccsch u ) = ArccschL u
convertFunction (Arcsech u ) = ArcsechL u
convertFunction (Arccoth u ) = ArccothL u
-}


convert :: RoseTree Expr -> ExprList
convert (Petal (Num n)) = NumL n
convert (Petal (Var x)) = VarL x
--convert (Petal (F f)) = G (convertFunction f)
convert (Briar AddOp es) = AddL (map convert es)
convert (Briar SubOp es) = SubL (map convert es)
convert (Briar MulOp es) = MulL (map convert es)
convert (Briar DivOp es) = DivL (map convert es)
convert (Briar PowOp (e:es)) = PowL [convert e] (map convert es)


e = x .* Num 3 .* Num 2 .* x .^ Num 9 .* (F (Sin x)) .+
    Num 2 .* x .* Num 8 .* ((F (Sin x)) .^ (x .^ Num 2)) .-
    (Num 2 .* x .* Num 8 .* ((F (Sin x)) .^ (x .^ Num 2)) .-
    Num 2 .* Num 7 .* x .* Num 3 .* Num 2 .* x .^ Num 9 .* (F (Sin x)) .-
    (Num 8 .* F (Cos x) .+
    (Num 3 .+ x) .* (F (Cos x)) ) )


flatten :: Op -> RoseTree Expr -> RoseTree Expr
flatten op tree = Briar op (flatten' op tree)
    where
    flatten' :: Op -> RoseTree Expr -> [RoseTree Expr]
    flatten' _ EmptyRose = [EmptyRose]
    flatten' _ (Petal x) = [Petal x]
    flatten' op (Briar briarOp (e:es))
        | op == briarOp = flatten' op e ++ (concatMap (flatten' op) es)
        | otherwise = [Briar briarOp (flatten' op e ++ (concatMap (flatten' op) es))]

-- precondition: must be passed flattened rose tree
roseToExpr :: RoseTree Expr -> Expr
roseToExpr (Petal (Num n)) = Num n
roseToExpr (Petal (Var x)) = Var x
roseToExpr (Petal (F f)) = F f
-- note we have Neg then one expression after it, so that's why I put no es after the last singleton.
roseToExpr (Briar SubOp ((EmptyRose) : [Petal p])) = Neg $ roseToExpr (Petal p)
roseToExpr (Briar SubOp ((EmptyRose) : [Briar AddOp es])) = Neg $ rebuild AddOp $ map roseToExpr es
roseToExpr (Briar SubOp ((EmptyRose) : [Briar SubOp es])) = Neg $ rebuild SubOp $ map roseToExpr es
roseToExpr (Briar SubOp ((EmptyRose) : [Briar MulOp es])) = Neg $ rebuild MulOp $ map roseToExpr es
roseToExpr (Briar SubOp ((EmptyRose) : [Briar DivOp es])) = Neg $ rebuild DivOp $ map roseToExpr es
roseToExpr (Briar SubOp ((EmptyRose) : [Briar PowOp es])) = Neg $ rebuild PowOp $ map roseToExpr es
roseToExpr (Briar AddOp es) = rebuild AddOp $ map roseToExpr es
roseToExpr (Briar SubOp es) = rebuild SubOp $ map roseToExpr es
roseToExpr (Briar MulOp es) = rebuild MulOp $ map roseToExpr es
roseToExpr (Briar DivOp es) = rebuild DivOp $ map roseToExpr es
roseToExpr (Briar PowOp es) = rebuild PowOp $ map roseToExpr es

rebuild :: Op -> [Expr] -> Expr
rebuild AddOp es = foldl1 (\acc x -> Add acc x) es
rebuild SubOp es = foldl1 (\acc x -> Sub acc x) es
rebuild MulOp es = foldl1 (\acc x -> Mul acc x) es
rebuild DivOp es = foldl1 (\acc x -> Div acc x) es
rebuild PowOp es = foldl1 (\acc x -> Pow acc x) es


-- rebuilds with add sub signs.
rebuildAS :: [Expr] -> Expr
rebuildAS es = foldl1 f es
    where
    f acc x
        | isNeg x = (Sub acc (getNeg x))
        | otherwise = Add acc x



toRose :: Tree Expr -> RoseTree Expr
toRose (Empty) = EmptyRose
toRose (Leaf x) = Petal x
toRose (Node op left right) = Briar (opStrToOp op) [toRose left, toRose right]
    where
    opStrToOp "+" = AddOp
    opStrToOp "-" = SubOp
    opStrToOp "*" = MulOp
    opStrToOp "/" = DivOp
    opStrToOp "^" = PowOp


mkTree :: Expr -> Tree Expr
mkTree (Var x) = Leaf (Var x)
mkTree (Num n) = Leaf (Num n)
mkTree (F func) = Leaf (F func) -- TODO should I show expressions in functino as a tree as well?
mkTree (Neg (Num n)) = Leaf (Num (-n))
mkTree (Neg e) = Node "-" Empty (mkTree e)
mkTree (Add e1 e2) = Node "+" (mkTree e1) (mkTree e2)
mkTree (Sub e1 e2) = Node "-" (mkTree e1) (mkTree e2)
mkTree (Mul e1 e2) = Node "*" (mkTree e1) (mkTree e2)
mkTree (Div e1 e2) = Node "/" (mkTree e1) (mkTree e2)
mkTree (Pow e1 e2) = Node "^" (mkTree e1) (mkTree e2)


splitR :: Op -> Expr -> [Expr]
splitR op expr = map roseToExpr [roseTree]
    where roseTree = flatten op $ toRose $ mkTree expr

getRList :: RoseTree Expr -> [RoseTree Expr]
getRList (Petal x) = [Petal x]
getRList (Briar _ es) = es



isSeparable :: Expr -> Bool
isSeparable expr = (length $ splitAS expr) > 1


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
    | otherwise = [e]




---------
isMono :: Expr -> Bool
isMono e
    | hasFunction e || isSeparable e = False
    | otherwise = foldl f True s'
    where
    -- e' = simplifyComplete e
    s' = map clean $ split MulOp e -- was e' , changed since got sent to infinite loop.
    f = (\acc x -> acc && (isNum x || isVar x || isPolyPow x))
    isPolyPow (Pow (Var _) (Neg (Num n))) = True
    isPolyPow (Pow (Var _) (Num n)) = True
    isPolyPow _ = False


isNegNum :: Expr -> Bool
isNegNum (Neg (Num _)) = True
isNegNum _ = False

isNumNeg :: Expr -> Bool
isNumNeg (Num n) = n < 0
isNumNeg _ = False



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
hasNeg (Num n) = n < 0
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
hasNegPow (Pow _ (Neg (Num n))) = n > 0
hasNegPow (Pow _ (Num n)) = n > 0
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


clean :: Expr -> Expr
clean expr
    | expr' == expr = expr
    | otherwise = cln expr'
    where
    expr' = cln expr
    cln (Num n) = Num n
    cln (Var x) = Var x
    cln (F f) = F f -- F $ fmap cln f
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

    -- note the neg pusher cases
    cln (Neg (Num n)) = Num (-n)
    cln (Neg (Neg e)) = cln e

    cln (Add (Neg a) (Neg b)) = Neg $ cln a .+ cln b
    cln (Add a (Neg b)) = cln a .- cln b
    cln (Add (Neg a) b) = Neg $ cln a .- cln b

-- TODO HELP later do this tedious work of simpliying negatives.
    -- cln (Sub a (Num n)) = if n < 0 then (cln a .+ Num (-1*n)) else (cln a .- Num n)
{-    cln (Sub a b)
        | isNegNumOrFrac a -}
    cln (Sub (Neg a) (Neg b)) = Neg $ cln a .- cln b
    cln (Sub a (Neg b)) = cln a .+ cln b
    cln (Sub (Neg a) b) = Neg $ cln a .+ cln b

    cln (Mul (Neg a) (Neg b)) = cln a .* cln b
    cln (Mul (Neg a) b) = Neg $ cln a .* cln b
    cln (Mul a (Neg b)) = Neg $ cln a .* cln b

    cln (Div (Neg a) (Neg b)) = cln a ./ cln b
    cln (Div (Neg a) b) = Neg $ cln a ./ cln b
    cln (Div a (Neg b)) = Neg $ cln a ./ cln b

    cln (Neg e) = Neg $ cln e
    {-
    cln (Neg (Mul e1 e2)) = Mul (cln (Neg e1)) (cln e2)
    cln (Neg (Div e1 e2)) = Div (cln (Neg e1)) (cln e2)
    cln (Neg (Pow base expo)) = Neg $ (cln base) .^ (cln expo)-}
    -- cln (Neg e) = Neg $ cln e

    cln (Add e1 e2) = cln e1 .+ cln e2
    cln (Sub e1 e2) = cln e1 .- cln e2
    cln (Mul e1 e2) = cln e1 .* cln e2
    cln (Div e1 e2) = cln e1 ./ cln e2
    cln (Pow e1 e2) = cln e1 .^ cln e2


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
countFunc c (F f) = c + 1
countFunc c (Neg e) = countFunc c e
countFunc c (Add e1 e2) = countFunc c e1 + countFunc c e2
countFunc c (Sub e1 e2) = countFunc c e1 + countFunc c e2
countFunc c (Mul e1 e2) = countFunc c e1 + countFunc c e2
countFunc c (Div e1 e2) = countFunc c e1 + countFunc c e2
countFunc c (Pow e1 e2) = countFunc c e1 -- + oneFunc count e2



------------------------------------------------

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


push :: ExprList -> Function -> FunctionL
push u (Sin v) = SinL u
push u (Cos v) = CosL u
push u (Tan v) = TanL u
push u (Csc v) = CscL u
push u (Sec v) = SecL u
push u (Cot v) = CotL u
push u (Arcsin v) = ArcsinL u
push u (Arccos v) = ArccosL u
push u (Arctan v) = ArctanL u
push u (Arccsc v) = ArccscL u
push u (Arcsec v) = ArcsecL u
push u (Arccot v) = ArccotL u
push u (Sinh v) = SinhL u
push u (Cosh v) = CoshL u
push u (Tanh v) = TanhL u
push u (Csch v) = CschL u
push u (Sech v) = SechL u
push u (Coth v) = CothL u
push u (Arcsinh v) = ArcsinhL u
push u (Arccosh v) = ArccoshL u
push u (Arctanh v) = ArctanhL u
push u (Arccsch v) = ArccschL u
push u (Arcsech v) = ArcsechL u
push u (Arccoth v) = ArccothL u
push u (E v) = EL u
push u (Ln v) = LnL u
push u (Log v1 v2) = LogL u u



isAddL :: ExprList -> Bool
isAddL (AddL _) = True
isAddL _ = False

isAdd :: Expr -> Bool
isAdd (Add _ _) = True
isAdd _ = False

isSub :: Expr -> Bool
isSub (Sub _ _) = True
isSub _ = False

isSubL :: ExprList -> Bool
isSubL (SubL _) = True
isSubL _ = False

isMul :: Expr -> Bool
isMul (Mul _ _) = True
isMul _ = False

isMulL :: ExprList -> Bool
isMulL (MulL _) = True
isMulL _ = False

isDiv :: Expr -> Bool
isDiv (Div _ _) = True
isDiv _ = False

isDivL :: ExprList -> Bool
isDivL (DivL _) = True
isDivL _ = False

getUpper :: Expr -> Expr
getUpper (Div numer _) = numer
getUpper _ = error "not div expr in getUpper"

getLower :: Expr -> Expr
getLower (Div _ denom) = denom
getLower _ = error "not div expr in getLower"

isPow :: Expr -> Bool
isPow (Pow _ _) = True
isPow _ = False

isPowL :: ExprList -> Bool
isPowL (PowL _ _) = True
isPowL _ = False

isNum :: Expr -> Bool
isNum (Num _) = True
isNum _ = False

isVar :: Expr -> Bool
isVar (Var _) = True
isVar _ = False

isNeg :: Expr -> Bool
isNeg (Neg e) = True
isNeg _ = False

getNeg :: Expr -> Expr
getNeg (Neg n) = n
getNeg _ = error "incorrect argument"

getNegL :: ExprList -> ExprList
getNegL (NegL n) = n
getNegL _ = error "incorrect argument"

