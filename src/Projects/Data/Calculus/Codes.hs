{-# LANGUAGE FlexibleContexts #-}
module Codes where


import Types

import Data.List
import Data.Char
import Data.Maybe
import Data.Ratio hiding (show)



------- UTIL --------------------------------------------------------------------------------------

-- note: global for fillZeroes function
oo = (Num 0, Num 0) -- vignette zeroes
o = Integer 0 -- zero


intToConst :: Int -> Int -> Const
intToConst num denom
    | denom == 1 = Integer num
    | otherwise = Quotient $ num % denom





-- TODO: replace the other elongate with this new one
-- Adds zeroes to the end of one of the lists inside the Code so they are the same length.
fillZeroes :: Code -> Code -> (Code, Code)
fillZeroes (Poly xs) (Poly ys) = (Poly $ fst res, Poly $ snd res)
    where res = filler o xs ys
fillZeroes (Trig xs) (Trig ys) = (Trig $ fst res, Trig $ snd res)
    where res = filler oo xs ys
fillZeroes (InvTrig xs) (InvTrig ys) = (InvTrig $ fst res, InvTrig $ snd res)
    where res = filler oo xs ys
fillZeroes (Hyperbolic xs) (Hyperbolic ys) = (Hyperbolic $ fst res, Hyperbolic $ snd res)
    where res = filler oo xs ys
fillZeroes (InvHyperbolic xs) (InvHyperbolic ys) = (InvHyperbolic $ fst res, InvHyperbolic $ snd res)
    where res = filler oo xs ys
fillZeroes c1@(Logarithmic _) c2@(Logarithmic _) = (c1, c2)
-- fillZeroes c1@(Exponential _) c2@(Exponential _) = (c1, c2)

-- Helper function for fillZeroes - acts as the structure for the fillZeroes function, the workhorse.
filler ::  a-> [a] -> [a] -> ([a], [a])
filler zero xs ys
   | len1 > len2 = (xs, ys ++ zeroes)
   | len1 < len2 = (xs ++ zeroes, ys)
   | otherwise = (xs, ys)
   where
       len1 = length xs
       len2 = length ys
       zeroes = replicate (abs (len1 - len2)) zero









---------------------------------------------------------------------------------------------------
--- CODES functions


-- PRECONDITION: After chisel().  Takes individual monomial terms (Consts and vars with Consts) that must have been
-- originally connected by Add, not Mul or Div. Expects chiselled input (no divs except if in PolyRational Code type).
-- POSTCONDITION: returns single Poly [] which contains the result of the entire string of added monomials.
addPoly :: Code -> Code -> Code
addPoly (Poly ps) (Poly qs) = Poly (zipWith (+) ps' qs')
    where (Poly ps', Poly qs') = fillZeroes (Poly ps) (Poly qs)
addPoly _ _ = Empty



{-

-- PRECONDITION: After chisel(). Takes individual monomial terms (Consts and vars with Consts) that must have been
-- originally connected by Mul only. Expects chiselled input (no divs except if in PolyRational Code type).
-- POSTCONDITION: returns single Poly [] which contains the result of the entire string of added monomials.
mulPoly :: Code -> Code -> Code
mulPoly (Poly ps) (Poly qs) = foldl1 addPoly products'
    where
    ts = zip ps [0..(length ps - 1)] -- ps can be rate type, but pows must be int type.
    maxPow = maximum $ map (\(Poly ps) -> length ps) products
    products = map (\(n, p) -> mulOnePoly n p qs) ts
    products' = map Poly $ map (\(Poly ps) -> ps ++ replicate (maxPow - length ps) 0) products


-- note n = Consts of poly, p = pow of poly with Consts n, q = pow of multiplied poly (accumulated)
-- (m:ms) = elements of other polynomial (added), acc = accumulated multiplications (is a list of
-- tuples that holds first the new Consts value and second the power of this Consts.
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

-}

{-
addCodes :: Code -> Code -> Code
addCodes p@(Poly _) q@(Poly _) = addPoly p q
addCodes t1@(Trig _) t2@(Trig _) = addTrig t1 t2
addCodes t1@(InvTrig _) t2@(InvTrig _) = addTrig t1 t2
addCodes h1@(Hyperbolic _) h2@(Hyperbolic _) = addTrig h1 h2
addCodes h1@(InvHyperbolic _) h2@(InvHyperbolic _) = addTrig h1 h2
addCodes l1@(Logarithmic _) l2@(Logarithmic _) = addLog l1 l2
addCodes _ _ = Empty

-}






