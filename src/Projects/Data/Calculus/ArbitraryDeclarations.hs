{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module ArbitraryDeclarations where


import Types
import Polynomial

import Data.List

import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, frequency)


instance Arbitrary Coeff where
    arbitrary = do
        int <- arbitrary
        frac <- arbitrary
        frequency [
            (1, return $ Whole int),
            (1, return $ Rational frac)]


instance Arbitrary Fraction where
    arbitrary = do
        ratio <- arbitrary
        return ratio


instance Arbitrary Expr where
    arbitrary = do
        e1 <- arbitrary
        e2 <- arbitrary
        coeff <- arbitrary
        func <- arbitrary
        frequency [
            (1, return $ Add e1 e2),
            (1, return $ Sub e1 e2),
            (1, return $ Mul e1 e2),
            (1, return $ Div e1 e2),
            (1, return $ Pow e1 e2),
            (1, return $ Neg e1),
            (1, return $ Num coeff),
            (1, return $ Var "x"), -- string variable x declared in types file.
            (1, return $ Func func)]


instance Arbitrary a => Arbitrary (Function a) where
    arbitrary = do
        x <- arbitrary
        b <- arbitrary
        frequency [
            (1, return $ Sin x),
            (1, return $ Cos x),
            (1, return $ Tan x),
            (1, return $ Csc x),
            (1, return $ Sec x),
            (1, return $ Cot x),
            (1, return $ Arcsin x),
            (1, return $ Arccos x),
            (1, return $ Arctan x),
            (1, return $ Arccsc x),
            (1, return $ Arcsec x),
            (1, return $ Arccot x),
            (1, return $ Sinh x),
            (1, return $ Cosh x),
            (1, return $ Tanh x),
            (1, return $ Csch x),
            (1, return $ Sech x),
            (1, return $ Coth x),
            (1, return $ Arcsinh x),
            (1, return $ Arccosh x),
            (1, return $ Arctanh x),
            (1, return $ Arccsch x),
            (1, return $ Arcsech x),
            (1, return $ Arccoth x),
            (1, return $ Ln x),
            (1, return $ Exp x),
            (1, return $ Log b x)]



instance Arbitrary Vignette where
    arbitrary = do
        exprTuples <- arbitrary
        return exprTuples

instance Arbitrary Code where
    arbitrary = do
        xs <- arbitrary
        expr <- arbitrary
        vignetteList <- arbitrary
        frequency [
            (1, return (Poly xs)),
            (1, return (Trig vignetteList)),
            (1, return (InverseTrig vignetteList)),
            (1, return (Hyperbolic vignetteList)),
            (1, return (InverseHyperbolic vignetteList)),
            (1, return (Exponential expr)),
            (1, return (Logarithmic vignetteList))]