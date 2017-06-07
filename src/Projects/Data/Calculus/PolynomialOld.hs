{-# LANGUAGE FlexibleContexts #-}
module PolynomialOld where

import Types

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



--makeFraction :: Int -> Fraction
makeFraction n = Rate $ n % 1


--meltExponent :: Expr -> Expr
meltExponent (Pow constBase expo) = Pow constBase (simplifyExprMultiVar expo)
meltExponent e = e


-- precondition: gets an expression with only polys of single variable.
-- postcondition: returns simplified version
meltPoly :: Expr -> Expr
meltPoly expr
    | isNothing mCode = fromJust mExpr
    | otherwise = decodePoly var (fromJust mCode)
    where
    vs = vars expr
    var = if (length vs == 1) then (Just (head vs)) else Nothing
    (mCode, mExpr) = codifyPoly expr var



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

{-

ee = Num 7 .* x .^ Num 2 .- Num (-3) .* Num 5 .* x .^ Num 8 .+ e7
ee7 = chisel $ distribute $ negExplicit ee
dive = (splitAS (chisel $ distribute $ negExplicit e7)) !! 0


exprs = splitAS $ chisel $ distribute $ negExplicit e7
(divsd, muls) = partition (\e -> isDiv e || isNegDiv e) exprs
(emPairs, cmPairs) = partition (\(cm,em) -> isJust em) (map codifyPolyD divsd)
mulsCodeFromMul = map codifyMono muls
mulsCodeFromDiv = catMaybes $ fst $ unzip cmPairs
mulsCode = foldl1 addPoly (mulsCodeFromMul ++ mulsCodeFromDiv)
divsExprFromDiv = rebuildAS $ catMaybes $ snd $ unzip emPairs
-}

-- TODO: when importing to other file, remember single var - use "x"
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
