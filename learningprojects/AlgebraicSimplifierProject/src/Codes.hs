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




instance Show Null where
    show Null = "?"

instance Encoded Monomial where
    add = addMono
    sub = subMono
    multiply = mulMono
    divide = divMono

instance Encoded Polynomial where
    add = addPoly
    sub = subPoly
    multiply = mulPoly
    divide = divPoly


instance (Encoded c) => Encoded (Trigonometric c) where
    add = addTrig -- adding trig and invtrig cases.
    sub = subTrig
    multiply = mulTrig
    divide = divTrig

instance (Encoded c) => Encoded (Hyperbolic c) where
    add = addHyper
    sub = subHyper
    multiply = mulHyper
    divide = divHyper

instance (Encoded c) => Encoded (Logarithmic c) where
    add = addLog
    sub = subLog
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







------------------------------
-- divPoly utils:


--------- coef methods ---------------------------
zero :: Const
zero = Whole 0

normalize :: [Const] -> [Int]
normalize coefs = map numerator normed -- getting normalized int numerator parts. Whole num.
    where nonzeroes = filter (/= zero) coefs -- removes zeroes in Poly []
          stripped = map get nonzeroes -- removing constructors, gets just Ratio Int types
          maxDenom = maximum $ map denominator stripped -- get max denom to normalize fracs.
          normed = mulCoefs stripped (maxDenom % 1) -- normalizing

mulCoefs :: [Fraction] -> Fraction -> [Fraction]
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
tupleToPoly (n, p) = Poly $ insert p n zeroes
    where zeroes = map Whole $ replicate (p + 1) 0











---------------------------------------------------------------------------------------------
------------------------------------------ MONOMIAL ---------------------------------------

-- PRECONDITION: powers must be equal
-- POSTCONDITION: added monomials
addMono :: Monomial -> Monomial -> Monomial
addMono (Mono (n, p)) (Mono (m, q)) = Mono (n + m, p)



-- PRECONDITION: powers must be equal
-- POSTCONDITION: subtracted monomials
subMono :: Monomial -> Monomial -> Monomial
subMono (Mono (n, p)) (Mono (m, q)) = Mono (n - m, p)



-- PRECONDITION: After chisel(). Takes individual monomial terms (Consts and vars with
-- Consts) that must have been originally connected by Mul only. Individual monomial terms
-- are represented by a list of Polynomials that have one coefficient.
-- POSTCONDITION: returns single Poly [] which contains the result of the entire string of
-- added monomials.
mulMono :: Monomial -> Monomial -> Monomial
mulMono (Mono (n, p)) (Mono (m, q)) = Mono (n * m, p + q)

-- PRECONDITION: After chisel, takes two monomials and divides them. PREC: p >= q.
-- POSTCONDITION: divided monomials, with power >= 0.
-- note: always Nothing in tuple because this type basket is meant for the divPoly outlier
divMono :: Monomial -> Monomial -> (Monomial, Maybe Monomial)
divMono (Mono (n, p)) (Mono (m, q)) = (Mono (n / m, p - q), Nothing)





---------------------------------------------------------------------------------------------
------------------------------------------ POLYNOMIAL ---------------------------------------


-- PRECONDITION: After chisel().  Takes individual monomial terms (Consts and vars with Consts)
-- that must have been originally connected by Add, not Mul or Div.
-- POSTCONDITION: returns single Poly [] which contains the result of the entire string of added
-- monomials. If the resulting ps list contains all zeroes or is empty, returns Poly []
addPoly :: Polynomial -> Polynomial -> Polynomial
addPoly (Poly ps) (Poly qs)
    | all (== zero) result = Poly []
    | otherwise = Poly result
    where
    (Poly ps', Poly qs') = fillZeroes (Poly ps) (Poly qs)
    result = Poly (zipWith (+) ps' qs')


subPoly :: Polynomial -> Polynomial -> Polynomial
subPoly (Poly ps) (Poly qs) = addPoly (Poly ps) (Poly $ map negate qs)


---------------------------------------------------------------------------------------------
-- PRECONDITION: After chisel(). Takes two polynomials (each poly contains monomials added).
-- POSTCONDITION: multiplies the polynomials term-by term like FOIL method.
mulPoly :: Polynomial -> Polynomial -> Polynomial
mulPoly (Poly ps) (Poly qs) = foldl1 addPoly products
    where
    ts = zip ps [0..(length ps - 1)] -- ps can be rate type, but pows must be int type.
    products = map (\(n, p) -> mulOnePoly n p qs) ts



-- precondition: n = Fraction poly-Const, p = poly pow, rs = list of poly Consts.
-- postcondition: takes result of mul() and wraps it up as Code type.
mulOnePoly :: Const -> Int -> [Const] -> Polynomial
mulOnePoly n p rs = Poly cs -- wrapping up types
    where
    ts = mul n p 0 rs [] -- note: getting result of single monomial multiplied by added monomials.
    maxPow = maximum $ map snd ts
    zzs = replicate (maxPow + 1) 0 -- making zero list long as highest poly pow.
    ccs = map (\(c,p) -> insert p c zzs) ts -- putting coefficients at correct pow-positions.
    cs = map sum (transpose ccs) -- flattening (adding coefs at locations)


-- precondition: takes n = poly Const, p = poly pow, q = accumulator poly pow,
-- (c:cs) = list of Consts, acc = accumulator poly Consts
-- postcondition: returns a list of tuples of (n,p) such that the given n and p in the
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
divPoly :: Polynomial -> Polynomial -> (Polynomial, Maybe Polynomial)
divPoly (Poly ps) (Poly qs)
    | null dp = (numeratorPoly, Nothing)
    | otherwise = (numeratorPoly, Just denominatorPoly)
    where
    -- dealing with coefficients.
    ps = map Whole [0,0,12,0,6,24]
    qs = map Whole [0,0,0,4,0,6]
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
    (Poly dp) = denominatorPoly







---------------------------------------------------------------------------------------------
------------------------------------------ TRIG ---------------------------------------------

-- PRECONDITION: the trigonometrics have same args and pows, otherwise would have
-- to return list of threeples or something.
-- POSTCONDITION: added result, so Trig[(x^2, 2),...] + Trig[(x^2, 2),...] = Trig [(2x^2,2)...]
addTrig :: Encoded c => Trigonometric c -> Trigonometric c -> (Trigonometric c, Maybe (Trigonometric c))
addTrig (Trig xs) (Trig ps) = Trig $ map addTups (zip xs ps)
addTrig (InvTrig xs) (InvTrig ps) = InvTrig $ map addTups (zip xs ps)


addTups ((x1,p1), (x2,p2)) = (add x1 x2, p1)
subTups ((x1,p1), (x2,p2)) = (sub x1 x2, p1)
mulTups ((x1,p1), (x2,p2)) = (x1, add p1 p2)
divTups ((x1,p1), (x2,p2)) = (x1, sub p1 p2) -- todo define sub or make Neg operator.

-- PRECONDITION: the trigonometrics have same args and can have different pows.
-- POSTCONDITION: multiplied results.
mulTrig :: Trigonometric c -> Trigonometric c -> (Trigonometric c, Maybe (Trigonometric c))
mulTrig (Trig xs) (Trig ps) = Trig $ map mulTups (zip xs ps)
mulTrig (InvTrig xs) (InvTrig ps) = InvTrig $ map mulTups (zip xs ps)


-- PRECONDITION: the trigonometrics have same args and can have different pows.
-- POSTCONDITION: divided results.
divTrig :: Trigonometric c -> Trigonometric c -> (Trigonometric c, Maybe (Trigonometric c))
divTrig (Trig xs) (Trig ps) = Trig $ map divTups (zip xs ps)
divTrig (InvTrig xs) (InvTrig ps) = InvTrig $ map divTups (zip xs ps)


------
-- TRIG UTILS


addTupls :: Encoded c => ((c, c), (c, c)) -> Maybe (c, c)
addTupls ((x1,p1), (x2,p2))
    | x1 == x2 && p1 == p2 = Just (add x1 x2, add p1 p2)
    | otherwise = Nothing


---------------------------------------------------------------------------------------------
------------------------------------------ HYPER --------------------------------------------


addHyper :: Hyperbolic c -> Hyperbolic c -> Hyperbolic c
addHyper (Hyper xs) (Hyper ys) = undefined
addHyper (InvHyper xs) (InvHyper ys) = undefined

mulHyper :: Hyperbolic c -> Hyperbolic c -> Hyperbolic c
mulHyper (Hyper xs) (Hyper ys) = undefined
mulHyper (InvHyper xs) (InvHyper ys) = undefined

divHyper :: Hyperbolic c -> Hyperbolic c -> (Hyperbolic c, Maybe (Hyperbolic c))
divHyper (Hyper xs) (Hyper ys) = undefined
divHyper (InvHyper xs) (InvHyper ys) = undefined


---------------------------------------------------------------------------------------------
--------------------------------------- LOGARITHM ------------------------------------------


addLog :: Logarithmic c -> Logarithmic c -> Logarithmic c
addLog = undefined

mulLog :: Logarithmic c -> Logarithmic c -> Logarithmic c
mulLog = undefined

divLog :: Logarithmic c -> Logarithmic c -> (Logarithmic c, Maybe (Logarithmic c))
divLog = undefined








