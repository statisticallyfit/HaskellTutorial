{-# LANGUAGE FlexibleContexts #-}
module Encoders where

import Types
import Main

import Util.Specific
import Util.General

------------------------------ Dealing with Polynomials ---------------------------------


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



-- precondition: takes chisel output which isDiv and which has no other div in the numerator
-- and denominator and no negpowers (that's why we can use codifypolyA.
 -- numerator can be separable but simplification only possible if denom not separable.
codifyPolyD :: Expr -> Maybe String -> (Maybe Code, Maybe Expr)
codifyPolyD expr var
    | isNothing div = (Nothing, Just $ Div (decodePoly var upper') (decodePoly var lower'))
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
codifyPoly :: Expr -> Maybe String -> (Maybe Code, Maybe Expr)
codifyPoly e var
    | isSeparable e && (all isMono es) = (Just $ foldl1 addPoly $ map codifyMono es, Nothing)
    | isSeparable e = (Just mulsCode, Just divsExprFromDiv)
    | hasDiv e && (isDiv expDiv) = codifyPolyD expDiv var
    | hasDiv e && (isMul expDiv) = (Just $ codifyPolyM expDiv, Nothing)
    | otherwise = (Just $ codifyPolyM expDiv, Nothing)
    where
    (Neg en) = e
    expDiv = chisel e
    es = splitAS expDiv
    (divs, muls) = partition (\e -> isDiv e || isNegDiv e) es
    (emPairs, cmPairs) = partition (\(cm,em) -> isJust em) (map ((flip codifyPolyD) var) divs)
    mulsCodeFromMul = map codifyMono muls
    mulsCodeFromDiv = catMaybes $ fst $ unzip cmPairs
    mulsCode = foldl1 addPoly (mulsCodeFromMul ++ mulsCodeFromDiv)
    divsExprFromDiv = rebuildAS $ catMaybes $ snd $ unzip emPairs




-- gets passed the var to put in the expression.
decodePoly :: Maybe String -> Code -> Expr
decodePoly var (Poly ps) = rebuildAS $ filter notZero (map clean polynomials)
    where
    -- note using x because poly is a number and we won't need it anyway.
    var' = if (isNothing var) then (Var "x") else (Var $ fromJust var)
    notZero x = (not (x == Num 0))
    ns = map Frac ps
    xs = replicate (length ns) var'
    pows = map Num $ [0 .. (length ns - 1)]
    xs' = zipWith Pow xs pows
    nxs = zipWith Mul ns xs'
    polynomials = filter notZero $ map (clean . negExplicit) nxs




------------------------ Dealing with Poly-Func (single func) ---------------------------


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
