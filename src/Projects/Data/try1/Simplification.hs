{-# LANGUAGE FlexibleContexts #-}
module Simplification where

import Types
import Encoder
import Toolset
import Internal




simplify' :: Expr -> Expr
simplify' (Pow base expo) = Pow (genVarSimplify base) (genVarSimplify expo)
simplify' (F f) = F $ fmap genVarSimplify f
simplify' expr = genVarSimplify expr

-- precondition: *** single variable *** simplification.
-- errors: TODO
-- 1) figure out how to combine the simplifymultivar with simplifysinglevar to make single simplify.
-- 2) represent RootPolys where pow = frac 1/3.
-- 3) handle each div
-- 4) handle non divs that need to be simplified: (x + 1)^3 (factor out when expo
-- is no bigger than 5, so 5 and under. If greater leave it as is.
-- 5) START HERE TOMORROW TODO: make polyroot instance of code and make all current
-- poly functions deal only with Nums not fractions, and polyroot to deal with fractions.
-- 6) if result has only polynomials or powers then return the highest polynomials first
-- then nums then powers.
-- 7) make sure to simplify powers correctly for e27 type exprs.
simplify :: Expr -> Expr
simplify (F f) = simplify' (F f)
simplify expr = finishExpr $ ps' .+ fs' .+ ffs' .+ divs' .+ (rebuildAS other''')
    where
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




-- note expecting the remains from other'' in above function (must be div)
-- precondition: get one non-separable div expr at a time.
-- HELP goes into infinite loop ebcause of simplify at the front - FIX TODO
divSimplify :: Expr -> Expr
divSimplify (Neg expr) = Neg $ divSimplify expr
divSimplify expr = {-simplify-} (Div (simplify up) (simplify lo))
    where (Div up lo) = expr



------------------------------ Dealing with Functions ---------------------------------

-- note separates function part from other parts and then simplifies functions.
-- precondition: expr cannot be separable (has to be glued) so takes one set of many funcs at time.
{-simplifyFunctions :: Expr -> Expr
simplifyFunctions (F f) = F f
simplifyFunctions (Mul f g)
    | sameArgs f g = (fmap simplify' f) .^ Num 2
    | otherwise = (fmap simplify' f) .* (fmap simplify' g)
simplifyFunctions e = e-}

------------------------------ Dealing with many functions ---------------------------------

-- note takes many sets of many funcs at a time. So expression can be separable.
-- note if expr is div (output from chisel) then we apply melt separately
-- postcondition returns separable function.
meltFunctions :: Expr -> Expr
meltFunctions (Num 0) = Num 0
meltFunctions e = e

------------------------ Dealing with Poly-Func (single func) ---------------------------

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


------------------------------ Dealing with Polynomials ---------------------------------

meltExpon :: Expr -> Expr
meltExpon (Pow constBase expo) = Pow constBase (simplify expo)


--- TODO make type constructor Polynomial that takes an expression or just plain type.
-- TODO make type Monomial
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
