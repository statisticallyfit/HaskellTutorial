{-# LANGUAGE FlexibleContexts #-}
module Util.Specific where

import Types
import Encoders
import Main
import Util.General


--------------------------------- Dealing with Functions ---------------------------------

sameArgs :: Expr -> Expr -> Bool
sameArgs (F f) (F g)
    | (getArg (F f)) == (getArg (F g)) = True
    | otherwise = False
sameArgs _ _ = False


--- TODO start here next time to finish off simplifyFunctions to identify if same constructor.
{-sameF :: Expr -> Expr -> Bool
sameF (F f) (F g) = all isSin fs || all isCos fs || all isTan fs || all isCsc fs
    || all isSec fs || all isCot fs || all isArcsin fs || all isArccos fs || all isArctan fs
    || all isArccsc fs || all isArcsec fs || all isArccot fs || all
    where fs = [F f, F g]-}


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


------------------------------ Dealing with many functions ---------------------------------


------------------------ Dealing with Poly-Func (single func) ---------------------------

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
    pluck (Var v) = Var v
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



-- note takes list of [Trig [..], InvTrig [], Trig []...] and gathers all same family
-- functions and adds them up. Gathers them in order: ts, its, hs, ihs, ls.
-- Returns list because we many have same family func left that are not addable.
addCodes :: [Code] -> [[Code]]
addCodes codes = map adder (gatherCodes codes)



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





------------------------------ Dealing with Polynomials ---------------------------------

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

-- note says if expression contains no functions and is just a polynomial term like 7x^2 / 6x or just 5x
-- needs to get input from chisel where either fully div or fully mul.
isPoly :: Expr -> Bool
isPoly e = ((not $ hasFunction e) || (all isMono (splitAll e))) && (not $ isExponential e)
