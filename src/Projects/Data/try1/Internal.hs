module Internal where

import Types


------
--- TODO make types Monomial and Polynomial
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
    where (divs, muls) = partition (\e -> isDiv e || isNegDiv e) (splitAS expr)


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
    | isDiv e || isMul e || isPow e = [e] -- TODO it's a mess here .. fix some other way.
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
    | isDiv e || isMul e || isPow e = [e]
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
    | isPow e = [e]
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
    | isPow e = [e]
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


isNegNumOrFrac :: Expr -> Bool
isNegNumOrFrac (Neg (Num _)) = True
isNegNumOrFrac (Neg (Frac _)) = True
isNegNumOrFrac _ = False

isNumOrFracNeg :: Expr -> Bool
isNumOrFracNeg (Num n) = n < 0
isNumOrFracNeg (Frac f) = f < 0
isNumOrFracNeg _ = False


isPow :: Expr -> Bool
isPow (Pow _ _) = True
isPow _ = False


isVar :: Expr -> Bool
isVar (Var _) = True
isVar _ = False

isNeg :: Expr -> Bool
isNeg (Neg e) = True
isNeg _ = False

isNum :: Expr -> Bool
isNum (Num _) = True
isNum (Neg (Num _)) = True
isNum _ = False



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