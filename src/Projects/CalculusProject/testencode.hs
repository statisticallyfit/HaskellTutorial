{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Data.Ratio



type Fraction = Ratio Int
data Const = Whole Int | Quotient Fraction deriving (Eq)



class Encoded c  where
    add :: c -> c -> c
    multiply :: c -> c -> c
    divide :: c -> c -> c


-- type Vignette c = (Encoded c, Encoded c) -- type synonym for readability -- TODO remove Encoded? leave c?

data Zero = Zero deriving (Eq)
data Monomial = Mono (Const, Const) deriving (Eq, Show)
data Polynomial = Poly [Const] deriving (Eq, Show)
data Trigonometric c = Trig [(c, c)] | InvTrig [(c, c)] deriving (Eq, Show)
data Hyperbolic c = Hyper [(c, c)] | InvHyper [(c, c)] deriving (Eq, Show)
data Logarithmic c = LogBase c c deriving (Eq, Show)

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
    add = addHyper -- adding trig and invtrig cases.
    multiply = mulHyper
    divide = divHyper

instance (Encoded c) => Encoded (Logarithmic c) where
    add = addLog -- adding trig and invtrig cases.
    multiply = mulLog
    divide = divLog



addMono :: Monomial -> Monomial -> Monomial
addMono = undefined

mulMono :: Monomial -> Monomial -> Monomial
mulMono = undefined

divMono :: Monomial -> Monomial -> Monomial
divMono = undefined

---
addPoly :: Polynomial -> Polynomial -> Polynomial
addPoly = undefined

mulPoly :: Polynomial -> Polynomial -> Polynomial
mulPoly = undefined

divPoly :: Polynomial -> Polynomial -> Polynomial
divPoly = undefined

---

addTrig :: Encoded a => Trigonometric a -> Trigonometric a -> Trigonometric a
addTrig = undefined

mulTrig :: Encoded a => Trigonometric a -> Trigonometric a -> Trigonometric a
mulTrig = undefined

divTrig :: Encoded a => Trigonometric a -> Trigonometric a -> Trigonometric a
divTrig = undefined

---
addHyper :: Encoded c => Hyperbolic c -> Hyperbolic c -> Hyperbolic c
addHyper (Hyper xs) (Hyper ys) = undefined
addHyper (InvHyper xs) (InvHyper ys) = undefined

mulHyper :: Encoded c => Hyperbolic c -> Hyperbolic c -> Hyperbolic c
mulHyper (Hyper xs) (Hyper ys) = undefined
mulHyper (InvHyper xs) (InvHyper ys) = undefined

divHyper :: Encoded c => Hyperbolic c -> Hyperbolic c -> Hyperbolic c
divHyper (Hyper xs) (Hyper ys) = undefined
divHyper (InvHyper xs) (InvHyper ys) = undefined


---

addLog :: Encoded c => Logarithmic c -> Logarithmic c -> Logarithmic c
addLog (LogBase b x) (LogBase c y) = undefined

mulLog :: Encoded c => Logarithmic c -> Logarithmic c -> Logarithmic c
mulLog (LogBase b x) (LogBase c y) = undefined

divLog :: Encoded c => Logarithmic c -> Logarithmic c -> Logarithmic c
divLog (LogBase b x) (LogBase c y) = undefined




------------------------------------------------------------------------------------------------------

-- todo error thrown here - why doesn't it recognize: Monomial ... Trigonometric as types of Encode?
{-
fillZeroes :: Encoded c => c -> c -> (c, c)
fillZeroes (Mono np) (Mono mq) = (Mono np, Mono mq)

fillZeroes (Poly xs) (Poly ys) = (Poly xs', Poly ys')
    where (xs', ys') = filler (Whole 0) xs ys

fillZeroes (Trig xs) (Trig ys) = (Trig xs', Trig ys')
    where (xs', ys') = filler (Zero, Zero) xs ys
fillZeroes (InvTrig xs) (InvTrig ys) = (InvTrig $ fst res, InvTrig $ snd res)
    where res = filler (Zero, Zero) xs ys

fillZeroes (Hyper xs) (Hyper ys) = (Hyper $ fst res, Hyper $ snd res)
    where res = filler (Zero, Zero) xs ys
fillZeroes (InvHyper xs) (InvHyper ys) = (InvHyper $ fst res, InvHyper $ snd res)
    where res = filler (Zero, Zero) xs ys

fillZeroes a@(LogBase _ _) b@(LogBase _ _) = (a, b)
-}



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
    show (Whole int) = show int
    show (Quotient q) = show q ++ ""


------------------------------------------------------------------------------------------------