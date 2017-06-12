{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Codes where



import Types

import GHC.Exts (Constraint)
import Data.List hiding (insert)
import Data.Char
import Data.Maybe
import Data.Ratio hiding (show)




instance Show Zero where
    show Zero = "0"

instance Encoded Monomial where
    add = addMono
    multiply = mulMono
    divide = divMono

instance Encoded Polynomial where
    add = addPoly
    multiply = mulPoly
    divide = divPoly


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

---------------------------------------------------------------------------------------------
---------------------------------------------- UTIL ------------------------------------------



get :: Const -> Fraction
get (Whole w) = w % 1
get (Quotient q) = q



fillZeroes :: Polynomial -> Polynomial -> (Polynomial, Polynomial)
fillZeroes (Poly xs) (Poly ys) = (Poly xs', Poly ys')
    where (xs', ys') = filler (Whole 0) xs ys



-- Helper function for fillZeroes - workhorse for the fillZeroes function,
filler ::  a -> [a] -> [a] -> ([a], [a])
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
-- length remaining the same. If index is not one of the positions inlist, throw error.
insert :: Int -> a -> [a] -> [a]
insert _ n [] = [n]
insert index n xs
    | index < 0 || index >= (length xs) = error "index error. "
    | otherwise = front ++ [n] ++ (tail back)
    where (front, back) = splitAt index xs
          newBack = if null back then [] else (tail back)



---------------------------------------------------------------------------------------------
------------------------------------------ MONOMIAL ---------------------------------------


-- todo will this type work?
addMono :: Monomial -> Monomial -> Monomial
addMono (Mono (n, p)) (Mono (m, q))
    | p == q = Mono (n + m, p)
    | otherwise = error "Cannot construct Monomial: powers are not equal."



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


-- PRECONDITION: After chisel().  Takes individual monomial terms (Consts and vars with Consts)
-- that must have been originally connected by Add, not Mul or Div.
-- POSTCONDITION: returns single Poly [] which contains the result of the entire string of added
-- monomials.
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
    ccs = map (\(c,p) -> insert p c zzs) ts -- putting coefficients at correct pow-positions.
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

-- PRECONDITION: takes top and bottom polynomial, cleaned after chisel() and no such thing as
-- Quotient (0 % 1) -- all normalized, clean input. Polynomial means also all integer powers.
-- POSTCONDITION: simplifies out common monomial terms in top and bottom.
divPoly :: Polynomial -> Polynomial -> Polynomial -- (Polynomial, Polynomial)
divPoly (Poly ps) (Poly qs) = (numeratorPoly) --, denominatorPoly)
    where
    -- dealing with coefficients.
    (ps', qs') = (normalize ps, normalize qs) -- normalizing poly coefs.
    coefGCD = foldl1 gcd (ps' ++ qs') -- find gcd of all the polycoefs.
    [ps'', qs''] = map ((flip divCoefs) coefGCD) [ps', qs'] -- divide polycoefs by gcd.
    [ps''', qs'''] = map (map Whole) [ps'', qs''] -- turning them to Consts (no more Quotients)
    -- dealing with powers.
    (ppows, qpows) = (getIndices ps, getIndices qs)
    minPow = minimum (ppows ++ qpows) --min pow in both top and bottom polys
    (ppows', qpows') = (subPows ppows minPow, subPows qpows minPow)
    -- wrapping up data
    numeratorPoly = foldl1 addPoly $ map tupleToPoly (zip ps''' ppows') -- making poly numerator
    denominatorPoly = foldl1 addPoly $ map tupleToPoly (zip qs''' qpows')

    --------- coef methods ---------------------------
    zero :: Const
    zero = Whole 0

    normalize :: [Const] -> [Int]
    normalize coefs = map numerator normed -- getting normalized int numerator parts. Whole num.
        where nonzeroes = filter (/= zero) coefs -- removes zeroes in Poly []
              stripped = map get nonzeroes -- removing constructors, gets just Ratio Int types
              maxDenom = maximum $ map denominator stripped -- get max denom to normalize fracs.
              normed = mulCoefs stripped (maxDenom % 1) -- normalizing

    mulCoefs :: [Fraction] -> Int -> [Int]
    mulCoefs coefs by = map (\c -> c * by) coefs -- postcondition: multiplying coefs by factor.

    divCoefs :: [Int] -> Int -> [Int]
    divCoefs coefs by = map (\c -> c `div` by) coefs -- postcondition: elements always whole numbers.
    --------- power methods ---------------------------
    getIndices :: [Const] -> [Int]
    getIndices xs = let powIndexPairs = zip xs [0..]
                        powIndexPairsNoZeroes = filter (\(p, i) -> p /= zero) powIndexPairs
                    in map snd powIndexPairsNoZeroes

    -- precondition: by num is never greater than any of the pows.
    -- postcondition: elements never negative.
    subPows :: [Int] -> Int -> [Int]
    subPows pows by = map (\p -> p - by) pows

    --------- wrapping-up data methods ---------------------------
    tupleToPoly :: (Const, Int) -> Polynomial
    tupleToPoly (n, p) = Poly $ insert n zeroes
        where zeroes = replicate p 0


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
    quotients = map Poly $ map (\(f,p) -> insert p f zs) ndpTriples


divOnePoly :: (Fraction, Int) -> (Fraction, Int) -> (Fraction, Int)
divOnePoly (Rate den, dPow) (Rate num, nPow) = (Rate $ (a * b) % (c * d), nPow - dPow)
    where (a, b, c, d) = (numerator num, denominator num, numerator den, denominator den)

-}





---------------------------------------------------------------------------------------------
------------------------------------------ TRIG ---------------------------------------------

-- TODO IMPLEMENT

addTrig :: Trigonometric c -> Trigonometric c -> Trigonometric c
addTrig (Trig xs) (Trig ys) = undefined -- TODO implement - shouldn't Encode be recursive for function arg?
addTrig (InvTrig xs) (InvTrig ys) = undefined

mulTrig :: Trigonometric c -> Trigonometric c -> Trigonometric c
mulTrig (Trig xs) (Trig ys) = undefined
mulTrig (InvTrig xs) (InvTrig ys) = undefined

divTrig :: Trigonometric c -> Trigonometric c -> Trigonometric c
divTrig (Trig xs) (Trig ys) = undefined
divTrig (InvTrig xs) (InvTrig ys) = undefined

---------------------------------------------------------------------------------------------
------------------------------------------ HYPER --------------------------------------------


-- TODO IMPLEMENT

addHyper :: Hyperbolic c -> Hyperbolic c -> Hyperbolic c
addHyper (Hyper xs) (Hyper ys) = undefined -- TODO implement - shouldn't Encode be recursive for function arg?
addHyper (InvHyper xs) (InvHyper ys) = undefined

mulHyper :: Hyperbolic c -> Hyperbolic c -> Hyperbolic c
mulHyper (Hyper xs) (Hyper ys) = undefined
mulHyper (InvHyper xs) (InvHyper ys) = undefined

divHyper :: Hyperbolic c -> Hyperbolic c -> Hyperbolic c
divHyper (Hyper xs) (Hyper ys) = undefined
divHyper (InvHyper xs) (InvHyper ys) = undefined


---------------------------------------------------------------------------------------------
--------------------------------------- LOGARITHM ------------------------------------------
-- TODO IMPLEMENT

addLog :: Logarithmic c -> Logarithmic c -> Logarithmic c
addLog = undefined

mulLog :: Logarithmic c -> Logarithmic c -> Logarithmic c
mulLog = undefined

divLog :: Logarithmic c -> Logarithmic c -> Logarithmic c
divLog = undefined








