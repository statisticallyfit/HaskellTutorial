{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Data.Ratio



type Fraction = Ratio Int
data Const = Integer Int | Quotient Fraction deriving (Eq)




data Zero = Zero deriving (Eq)
data Monomial = Mono (Const, Const) deriving (Eq, Show)
data Polynomial = Poly [Const] deriving (Eq, Show)
data Trigonometric c = Trig [(c, c)] | InvTrig [(c, c)] deriving (Eq, Show)
data Hyperbolic c = Hyper [(c, c)] | InvHyper [(c, c)] deriving (Eq, Show)
data Logarithmic c = LogBase c c deriving (Eq, Show)

instance Show Zero where
    show Zero = "0"


addMono :: Encoded Monomial -> Encoded Monomial -> Encoded Monomial
addMono = undefined

mulMono :: Encoded Monomial -> Encoded Monomial -> Encoded Monomial
mulMono = undefined

divMono :: Encoded Monomial -> Encoded Monomial -> Encoded Monomial
divMono = undefined

---

addPoly :: Encoded Polynomial -> Encoded Polynomial -> Encoded Polynomial
addPoly = undefined

mulPoly :: Encoded Polynomial -> Encoded Polynomial -> Encoded Polynomial
mulPoly = undefined

divPoly :: Encoded Polynomial -> Encoded Polynomial -> Encoded Polynomial
divPoly = undefined

---

addTrig :: Encoded (Trigonometric a) -> Encoded (Trigonometric a) -> Encoded (Trigonometric a)
addTrig = undefined

mulTrig :: Encoded (Trigonometric a) -> Encoded (Trigonometric a) -> Encoded (Trigonometric a)
mulTrig = undefined

divTrig :: Encoded (Trigonometric a) -> Encoded (Trigonometric a) -> Encoded (Trigonometric a)
divTrig = undefined

---
addHyper :: Encoded (Hyperbolic c) -> Encoded (Hyperbolic c) -> Encoded (Hyperbolic c)
addHyper = undefined

mulHyper :: Encoded (Hyperbolic c) -> Encoded (Hyperbolic c) -> Encoded (Hyperbolic c)
mulHyper = undefined

divHyper :: Encoded (Hyperbolic c) -> Encoded (Hyperbolic c) -> Encoded (Hyperbolic c)
divHyper = undefined

---

addLog :: Encoded (Logarithmic c) -> Encoded (Logarithmic c) -> Encoded (Logarithmic c)
addLog = undefined

mulLog :: Encoded (Logarithmic c) -> Encoded (Logarithmic c) -> Encoded (Logarithmic c)
mulLog = undefined

divLog :: Encoded (Logarithmic c) -> Encoded (Logarithmic c) -> Encoded (Logarithmic c)
divLog = undefined




------------------------------------------------------------------------------------------------------

-- todo error thrown here - why doesn't it recognize: Monomial ... Trigonometric as types of Encode?

--fillZeroes :: Encoded c => c -> c -> (c, c)
fillZeroes (Mono np) (Mono mq) = (Mono np, Mono mq)

fillZeroes (Poly xs) (Poly ys) = (Poly xs', Poly ys')
    where (xs', ys') = filler (Integer 0) xs ys

fillZeroes (Trig xs) (Trig ys) = (Trig xs', Trig ys')
    where (xs', ys') = filler (Zero, Zero) xs ys
fillZeroes (InvTrig xs) (InvTrig ys) = (InvTrig $ fst res, InvTrig $ snd res)
    where res = filler (Zero, Zero) xs ys

fillZeroes (Hyper xs) (Hyper ys) = (Hyper $ fst res, Hyper $ snd res)
    where res = filler (Zero, Zero) xs ys
fillZeroes (InvHyper xs) (InvHyper ys) = (InvHyper $ fst res, InvHyper $ snd res)
    where res = filler (Zero, Zero) xs ys

fillZeroes a@(LogBase _ _) b@(LogBase _ _) = (a, b)




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







------------------------------------------------------------------------------------------------


instance  {-# OVERLAPPING #-} Show Fraction where
    show ratio
        | numerator ratio == 0 = "0"
        | denominator ratio == 1 = show (numerator ratio)
        | otherwise = (show (numerator ratio)) ++ "/" ++ (show (denominator ratio))



------------------------------------------------------------------------------------------------
-- Const Instances



instance Show Const where
    show (Integer int) = show int
    show (Quotient q) = show q ++ ""


------------------------------------------------------------------------------------------------