{-# LANGUAGE FlexibleContexts #-}
module Codes where


import Types

import Data.List
import Data.Char
import Data.Maybe
import Data.Ratio hiding (show, Rational)
import Prelude hiding (Rational)



------- UTIL --------------------------------------------------------------------------------------

-- note: global for fillZeroes function
oo = (Num 0, Num 0) -- vignette zeroes
o = Integer 0 -- zero


intToConst :: Int -> Int -> Const
intToConst num denom
    | denom == 1 = Integer num
    | otherwise = Quotient $ num % denom


rationalToConst :: Rational -> Const
rationalToConst rat
    | denominator rat == 1 = Integer $ numerator rat
    | otherwise = Quotient rat


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




-- PRECONDITION: After chisel(). Takes individual monomial terms (Consts and vars with Consts) that must have been
-- originally connected by Mul only. Expects chiselled input (no divs except if in PolyRational Code type).
-- POSTCONDITION: returns single Poly [] which contains the result of the entire string of added monomials.


mulPoly :: Code -> Code -> Code
mulPoly (Poly ps) (Poly qs) = foldl1 addPoly products'
    where
    ts = zip ps [0..(length ps - 1)] -- ps can be rate type, but pows must be int type.
    products = map (\(n, p) -> mulOnePoly n p qs) ts
    maxPow = maximum $ map (\(Poly ps) -> (length ps - 1)) products
    products' = map Poly $ map (\(Poly ps) -> ps ++ replicate (maxPow - length ps) 0) products
mulPoly _ _ = Empty


-- PRECONDITION: n = Rational poly-Const, p = poly pow, rs = list of poly Consts.
-- POSTCONDITION: takes result of mul() and wraps it up as Code type.
mulOnePoly :: Const -> Int -> [Const] -> Code
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


put :: Int -> a -> [a] -> [a]
put _ n [] = [n]
put index n xs
    | index < 0 = error "index is negative "
    | otherwise = front ++ [n] ++ (tail back)
    where (front, back) = splitAt index xs
          newBack = if null back then [] else (tail back)


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






