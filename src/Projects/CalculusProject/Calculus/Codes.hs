{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleInstances #-}
module Codes where



import Types

import GHC.Exts (Constraint)
import Data.List
import Data.Char
import Data.Maybe
import Data.Ratio hiding (show)



instance Encoded Monomial where
    add = addMono
    multiply = mulMono
    divide = divMono

instance Encoded Polynomial where
    add = addPoly
    multiply = mulPoly
    divide = divPoly

{-
instance (Encoded c) => Encoded (Trigonometric c) where
    add = addTrig -- adding trig and invtrig cases.
    multiply = mulTrig
    divide = divTrig

instance (Encoded c) => Encoded (Hyperbolic c) where
    add = addHyper
    multiply = mulHyper
    divide = divHyper

instance (Encoded c) => Encoded (Logarithmic c) where
    add = addLog
    multiply = mulLog
    divide = divLog

-}

---------------------------------------------------------------------------------------------
---------------------------------------------- UTIL ------------------------------------------

-- note: global for fillZeroes function





-- TODO: replace the other elongate with this new one
-- Adds zeroes to the end of one of the lists inside the Code so they are the same length.
fillZeroes :: Encoded c => c -> c -> (c, c)
fillZeroes (Mono np) (Mono mq) = (Mono np, Mono mq)
fillZeroes (Poly xs) (Poly ys) = (Poly $ fst res, Poly $ snd res)
    where res = filler (Integer 0) xs ys
{-
fillZeroes (Trig xs) (Trig ys) = (Trig $ fst res, Trig $ snd res)
    where res = filler oo xs ys
fillZeroes (InvTrig xs) (InvTrig ys) = (InvTrig $ fst res, InvTrig $ snd res)
    where res = filler oo xs ys
fillZeroes (Hyper xs) (Hyper ys) = (Hyper $ fst res, Hyper $ snd res)
    where res = filler oo xs ys
fillZeroes (InvHyper xs) (InvHyper ys) = (InvHyper $ fst res, InvHyper $ snd res)
    where res = filler oo xs ys
fillZeroes (LogBase v) (LogBase w) = (LogBase v, LogBase w)

-}

-- Helper function for fillZeroes - workhorse for the fillZeroes function,
filler ::  a-> [a] -> [a] -> ([a], [a])
filler zero xs ys
   | len1 > len2 = (xs, ys ++ zeroes)
   | len1 < len2 = (xs ++ zeroes, ys)
   | otherwise = (xs, ys)
   where
       len1 = length xs
       len2 = length ys
       zeroes = replicate (abs (len1 - len2)) zero





-- PRECONDITION: index = index where to but a number in a list, n = the number, xs = the list.
-- POSTCONDITION: returns a list that contains the number n at position index in xs, with
-- length increased by 1. If index is not one of the positions inlist, throw error.
put :: Int -> a -> [a] -> [a]
put _ n [] = [n]
put index n xs
    | index < 0 || index >= (length xs) = error "index error. "
    | otherwise = front ++ [n] ++ (tail back)
    where (front, back) = splitAt index xs
          newBack = if null back then [] else (tail back)



---------------------------------------------------------------------------------------------
------------------------------------------ MONOMIAL ---------------------------------------


-- todo will this type work?
addMono :: Encoded c => Monomial -> Monomial -> c
addMono (Mono (n, p)) (Mono (m, q))
    | p == q = Mono (n + m, p)
    | otherwise = Poly [0] -- TODO IMPLEMENT HERE TO MAKE STRING OF MONOMIALS = POLY



-- PRECONDITION: After chisel(). Takes individual monomial terms (Consts and vars with
-- Consts) that must have been originally connected by Mul only. Individual monomial terms
-- are represented by a list of Polynomials that have one coefficient.
-- POSTCONDITION: returns single Poly [] which contains the result of the entire string of
-- added monomials.
mulMono :: Monomial -> Monomial -> Monomial
mulMono (Mono (n, p)) (Mono (m, q)) = Mono (n * m, p + q)


divMono :: Monomial -> Monomial -> Monomial
divMono (Mono (n, p)) (Mono (m, q)) = Mono (n / m, p - q)


---------------------------------------------------------------------------------------------
------------------------------------------ POLYNOMIAL ---------------------------------------


-- PRECONDITION: After chisel().  Takes individual monomial terms (Consts and vars with Consts) that must have been
-- originally connected by Add, not Mul or Div.
-- POSTCONDITION: returns single Poly [] which contains the result of the entire string of added monomials.
addPoly :: Polynomial -> Polynomial -> Polynomial
addPoly (Poly ps) (Poly qs) = Poly (zipWith (+) ps' qs')
    where (Poly ps', Poly qs') = fillZeroes (Poly ps) (Poly qs)


---------------------------------------------------------------------------------------------
-- PRECONDITION: After chisel(). Takes two polynomials (each poly contains monomials added).
-- POSTCONDITION: multiplies the polynomials term-by term like FOIL method.
mulPoly :: Polynomial -> Polynomial -> Polynomial
mulPoly (Poly ps) (Poly qs) = foldl1 addPoly products
    where
    ts = zip ps [0..(length ps - 1)] -- ps can be rate type, but pows must be int type.
    products = map (\(n, p) -> mulOnePoly n p qs) ts



-- PRECONDITION: n = Fraction poly-Const, p = poly pow, rs = list of poly Consts.
-- POSTCONDITION: takes result of mul() and wraps it up as Code type.
mulOnePoly :: Const -> Int -> [Const] -> Polynomial
mulOnePoly n p rs = Poly cs -- wrapping up types
    where
    ts = mul n p 0 rs [] -- note: getting result of single monomial multiplied by added monomials.
    maxPow = maximum $ map snd ts
    zzs = replicate (maxPow + 1) 0 -- making zero list long as highest poly pow.
    ccs = map (\(c,p) -> put p c zzs) ts -- putting coefficients at correct pow-positions.
    cs = map sum (transpose ccs) -- flattening (adding coefs at locations)



-- PRECONDITION: takes n = poly Const, p = poly pow, q = accumulator poly pow,
-- (c:cs) = list of Consts, acc = accumulator poly Consts
-- POSTCONDITION: returns a list of tuples of (n,p) such that the given n and p in the
-- parameters are multiplied as a poly-pow against the values in the acc list.
-- Simply: takes a single monomial and multiplies it against a string of added monomials.
mul :: (Eq a, Num a, Num b) => a -> b -> b -> [a] -> [(a, b)] -> [(a, b)]
mul _ _ _ [] acc = acc
mul 0 _ _ _ acc = [(0, 0)] ++ acc
mul n p q (c:cs) acc
    | n * c == 0 = mul n p (q + 1) cs acc
    | otherwise = mul n p (q + 1) cs (acc ++ [(n * c, p + q)])


---------------------------------------------------------------------------------------------


divPoly :: Polynomial -> Polynomial -> Polynomial
divPoly (Poly ps) (Poly qs) = Poly ps  -- TODO edit this function

{-
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

-}





---------------------------------------------------------------------------------------------
------------------------------------------ TRIG ---------------------------------------------

-- TODO IMPLEMENT
{-
addTrig :: Trigonometric c -> Trigonometric c -> Trigonometric c
addTrig (Trig xs) (Trig ys) = Trig xs -- TODO implement - shouldn't Encode be recursive for function arg?
addTrig (InvTrig xs) (InvTrig ys) = InvTrig xs

mulTrig :: Trigonometric c -> Trigonometric c -> Trigonometric c
mulTrig (Trig xs) (Trig ys) = Trig xs
mulTrig (InvTrig xs) (InvTrig ys) = InvTrig xs

divTrig :: Trigonometric c -> Trigonometric c -> Trigonometric c
divTrig (Trig xs) (Trig ys) = Trig xs
divTrig (InvTrig xs) (InvTrig ys) = InvTrig xs

---------------------------------------------------------------------------------------------
------------------------------------------ HYPER --------------------------------------------


-- TODO IMPLEMENT

addHyper :: Hyperbolic c -> Hyperbolic c -> Hyperbolic c
addHyper (Hyper xs) (Hyper ys) = Hyper xs -- TODO implement - shouldn't Encode be recursive for function arg?
addHyper (InvHyper xs) (InvHyper ys) = InvHyper xs

mulHyper :: Hyperbolic c -> Hyperbolic c -> Hyperbolic c
mulHyper (Hyper xs) (Hyper ys) = Hyper xs
mulHyper (InvHyper xs) (InvHyper ys) = InvHyper xs

divHyper :: Hyperbolic c -> Hyperbolic c -> Hyperbolic c
divHyper (Hyper xs) (Hyper ys) = Hyper xs
divHyper (InvHyper xs) (InvHyper ys) = InvHyper xs


---------------------------------------------------------------------------------------------
--------------------------------------- LOGARITHM ------------------------------------------
-- TODO IMPLEMENT

addLog :: Logarithmic c -> Logarithmic c -> Logarithmic c
addLog l@(LogBase (v1, v2)) (LogBase (w1, w2)) = l

mulLog :: Logarithmic c -> Logarithmic c -> Logarithmic c
mulLog l@(LogBase (v1, v2)) (LogBase (w1, w2)) = l

divLog :: Logarithmic c -> Logarithmic c -> Logarithmic c
divLog l@(LogBase (v1, v2)) (LogBase (w1, w2)) = l


-}





