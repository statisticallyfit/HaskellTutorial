{-# LANGUAGE FlexibleContexts #-}
module Tests where

import Types
import Encoders
import Extensions
import Main
import Utils
import UtilsGeneral

e1 :: Expr
e1 = Num(4) .* x .* (F (Sin x)) .* Num(2) .* (F (Cos x)) .* Num(5) .* Num(2) .* Num(3) .* (F (Tan x))
e2 = e1 .+ Num(2) .* x .+ Num(7) .* x
e3 = Num(4) .* x .+ Num(3) .* x .^ Num(5) .- (F (Sin (Num(3) .+ x)))
e4 = Neg(F (Sin x)) .* Neg(Num(2))
e5 = Neg (Num(4) .* x .+ Num(2) .* y)
e6 = Num (-7) .* x .^ Num 2 .+ Num 3 .* x .+ Num 4 .* x .+ Num 5 .* x .^ Num 2 .-
    Num 3 .* (F $ Sin $ Num 4 .* x) .+ Num 5 .* (F $ Cos x) .+ Num 2 .+ Num 3
e7 = (Num 3 .* x .^ Num 3) ./ (Num 3 .* x .^ (Num 1 ./ Num 3)) .-
    (Num 8 .* x .^ Num 9) ./ (Num 4 .* x .^ Num 3)
e7' = (Num 3 .* x .^ Num 3) ./ ((Num 3 .* x .^ (Num 1 ./ Num 3)) .-
    (Num 8 .* x .^ Num 9)) ./ (Num 4 .* x .^ Num 3)
e8 = e6 .+ e7
-- NOTE IMPORTANT you must put (e1 * e2) / (e3 * e4) brakcets like that because otherwise
-- error says you cannot mix ./ and .* because one is infix l and other is infix r.
e9 = ((Num 3 .* x .^ Num 3) ./ (Num 3 .* x .^ (Num 1 ./ Num 3))) .*
     ((Num 8 .* x .^ Num 9) ./ (Num 4 .* x .^ Num 3))
e10 = (Num 3 .* x .^ Num 3) ./ ((Num 3 .* x .^ (Num 1 ./ Num 3)) .*
     (Num 8 .* x .^ Num 9)) ./ (Num 4 .* x .^ Num 3)
e11 = Num 4 .* (x .+ Num 3)
-- TODO fix show for this one using numTerms.
e12 = ((F (Sin x)) .+ (F (Cos x)) .* Num 4 .* x .^ Num 2) .^ (x .+ Num 5 .- (F (Tan (Num 2 .* x))))
e13 = Num 4 .* x .+ Num 3 .* x .+ x .+ Num 6 .* x .^ Num 7 .+ Num 2 .+ Num 3
e14 = Num 10 .* x .- (F (Sin x)) .- (F (Cos (Num 2 .* x))) .+ x .- y .- Num 5 .- Num 8 .- Num 9 .+ x
e15 = F (Sin (x .+ Num 3 .+ Num 4 .+ Num 8 .* x .- y .+ Num 2 .^ x))
e16 = Num 2 .* Num 8 .* Num 2 .* x .^ Num (-7) .* Num 3 .* x .* (F (Sin (x .^ Num (-8))))
e17 = Num 2 .* x .^ Num (-7) .* (Num 3 .+ x) .* (F (Sin (x .^ Num (-8))))
-- NOTE to test poly encoding and decoding
e18 = ( ( (x .+ Num 1) .^ Num 3) ./ (x .+ Num 1))
e19 = ( ( (x .+ Num 1) .^ Num 3) ./ ((x .+ Num 1) .^ Num 4))
e20 = ( ( (x .+ Num 1) .^ Num (-3)) ./ ((x .+ Num 1) .^ Num 4))
e21 = Num 3 .* x .^ Num 2 .* Num 4 .* x .^ Num (-3) .+ Num 5 .* x .-
    ( ( (x .+ Num 1) .^ Num 3) ./ (x .+ Num 2)) .+ (x .+ Num 1) .^ Num 3 .+
    Num 5 .* x .^ Num 3 .* Num 6 .* x .^ Num 2 .- ((Num 4 .* x) ./ (x .+ Num 1))
e22 = (Num 3 .* x .^ Num (-2) .* Num 4 .* x .^ Num 3 .+ Num 5 .* x .^ Num 4 .* (x .+ Num 1) .^ Num 6) ./
    (x .+ Num 1)
e23 = (Num 3 .* x .^ Num (-2) .* Num 4 .* x .^ Num 3 .+ Num 5 .* x .^ Num 4 .* (x .+ Num 1) .^ Num 6) ./
    (Num 8 .* x .^ Num 3) .* (F (Sin (Num 2 .* x)))
e24 = (Num 3 .* x .^ Num (-2) .* Num 4 .* x .^ Num 3 .+
    Num 3 .* x .^ Num (-8) .* Num 5 .* x .^ Num 4 .* (x .+ Num 1) .^ Num 6) ./
    (Num 8 .* x .^ Num 3)  .* (F (Tan (Num (-3) .* x .^ Num 2)))
-- note test unjoin polyfunc
e25 = Num 4 .* x .^ Num 7 .* x .* Num 8 ./ (Num 4 .* x .* F (Sin x)) .^ Num 4 .* x .* Num 3
e26 = Num 4 .* x .^ Num 7 .* x .* Num 8 ./ (Num 4 .* x) .* F (Sin x) .^ Num 4 .* x .* Num 3
-- note test unjoiner and codifypolyfunc
e27 = (Num 4 .* x .+ Num 5 .* x .* Num 6 .* x .^ Num 8) ./
    (Num 5 .* x .* ((Num 4 .* x .+ Num 3 .* F (Sin x)) .^ Num 7))

-- testing meltpolyfunc
pf1 = (Num 4 .* x  .- Num 5 .* x .^ (x ./ (Num 3 .+ Num 2))) .* (F (Sin x)) .* Num 8 .* x
pf2 = (Num 4 .* x .+ Num 33 .* x .^ Num (-8)) ./ (Num 4 .* x .* F (Cos x) .+ Num 9 .* x)
pf3 = (Num 4 .* x .* F (Cos x) .+ Num 9 .* x) ./ (Num 4 .* x .+ Num 33 .* x .^ Num (-8))
pf4 = (Num 4 .* (x .* F (Cos x)) .^ Num 11 .+ Num 9 .* x) ./ (Num 4 .* x .+ Num 33 .* x .^ Num (-8))
pf5 = (Num 4 .* x .+ Num 33 .* x .^ Num (-8)) ./ (Num 4 .* (x .* F (Cos x)) .^ Num 11 .+ Num 9 .* x)
pf6 = (Num 4 .* x .- Num 33 .* x .^ Num (-8)) ./ (Num 4 .* (x .* F (Cos x)) .^ Num 11 .+ Num 9 .* x)
pf7 = (Num 4 .* x .- Num (-33) .* x .^ Num (-8)) ./ (Num 4 .* (x .* F (Cos x)) .^ Num 11 .+ Num 9 .* x)

e = x .* Num 3 .* Num 2 .* x .^ Num 9 .* (F (Sin x)) .+
    Num (-2) .* x .* Num 8 .* ((F (Sin x)) .^ (x .^ Num 2)) .-
    (Num 2 .* x .* Num 8 .* ((F (Sin x)) .^ (x .^ Num 2)) .-
    Num 2 .* Num 7 .* x .* Num 3 .* Num 2 .* x .^ Num 9 .* (F (Sin x)) .-
    (Num 8 .* F (Cos x) .-
    (Num 3 .+ x) .* (F (Cos x)) ) )

-- testing meltpoly func how it does with inner functions inside powers.
pfhard = (x .^ Num 2) ./ (Num 5 .* x .* (Num 4 .* x .+ F (Sin x)) .^ Num 7)
pfharder = (x .^ Num 2) ./ (Num 5 .* x .* (Num 4 .* x .+ (F (Sin x) .- Num 8) .^ Num 22) .^ Num 7)


-- testing addCodes with adder function

ys1 = [(Num 4,x,x .^ Num 2), (Num 6, Num 5, x .^ Num 3), (Num (-10),x,x .^ Num 2),
    (Num 2, x, x .^ Num 2), (x .+ Num 1 , Num 5, x .^ Num 3), (Num 22, Num 5, x .^ Num 3)]

ys2 = [(Num (-8),x,x .^ Num 2), (x .^ Num 7 .+ Num 3 .* x .^ Num 2, Num 5, x .^ Num 3),
    (Num 4, Num 4, x .^ Num 4), (Num 2, x, x .^ Num 2), (Num 1,Num 5,x .^ Num 3),
    (Num 1, Num 4, x .^ Num 4)]

ys3 = [(Num (-20),x,x .^ Num 2), (Num 5, Num 5, x .^ Num 3), (Num 2, Num 5, x .^ Num 3),
    (Num 1,Num 4, x .^ Num 4), (Num (-2),Num 4, x .^ Num 4), (Num 19, Num 5, x .^ Num 3)]

ys4 = [(Num 88, x, x .^ Num 2), (Num 90 .+ x .^ Num 3 .- Num 3 .* x .^ Num 33, x, x .^ Num 2),
    (Num 44, x, x .^ Num 2), (Num 11 .+ x .- x .^ Num 6, Num 5, x .^ Num 3),
    (Num 12, Num 4, x .^ Num 4), (Num 5, Num 5, x .^ Num 3)]

ys5 = [(Num 7,x,x .^ Num 2), (Num (-54), Num 5, x .^ Num 3), (Num 14, Num 4, x .^ Num 4),
    (Num 44, Num 4, x .^ Num 4), (Num 3, Num 4, x .^ Num 4), (Num (-8) .* x, Num 5, x .^ Num 3)]

ys = concat $ [map Trig [ys1, ys2, ys3, ys4, ys5]] ++ [map InvHyp [ys3, ys5, ys2]] ++
    [map Hyperbolic [ys1,ys2, ys5, ys4]] ++ [map Logarithmic [ys5, ys3, ys2, ys1, ys4]] ++
    [map Trig [ys1, ys5, ys3]]  ++ [map Logarithmic [ys2, ys4, ys2, ys4]] ++
    [map InvHyp [ys1, ys1, ys2, ys3, ys4, ys4, ys1]]

--- testing meltExpon
e28 = (Pow (Neg (Num 3)) (x .+ Num 1 .+ Num 3 .+ Num 2 .* x))







ee = concatMap (split MulOp) $ splitAS vss

-- note general input to general var split function.
vss = x .^ Num 2 .+ Num 5 .* x .^ Num 8 .- y .* F (Sin x) .- Num 2 .* x .+ x .+ y .-
    Num 8 .* y .^ Num 2 .+ x .- Num 2 .* y .+ x .* y .* z .* Num 4 .+ z .+ z .* Num 5 .+
    Num 6 .* z .+ F (Sin (y .^ Num 2 .* z .* z .^ Num 6 .* Num 3)) .-
    F (Cos z) .* z .* Num 4 .- z .- Num 8 .* y .- z .+ z .* F (Tan (x .* y)) .+
    z .^ Num 9 .+ x .* y .^ Num 3 .* z .^ Num 9 .* x .* z .* y .* Num 2 .* Num 7 .+
    Num 4 .* z .* F (Sin z) .- x .* y .- x .* z .^ Num 7 .+ Num 4 .* y

-- note meant to be interpreted as  mulsplits just for varmulsplit func testing
vs = splitAS $ x .^ Num 2 .+ Num 5 .* x .^ Num 8 .- Num 2 .* x .+ x .+ y .-
    Num 8 .* y .^ Num 2 .+ x .- Num 2 .* y .+ y .* Num 4 .+ z .+ z .* Num 5 .+ Num 6 .* z .+
    z .- Num 8 .* y .- z .+ z .^ Num 9 .+ x .+ Num 4 .* z .- x .- x .^ Num 7 .+ Num 4 .* y




-- to test with mulexplicit
m1 = (Num 4 ./ Num 3) ./ Num 9 -- should be (4/3) * (1/9)

-- to test with divexplicit
d1 = (Num 1 ./ Num 2) .* Num 4
d2 = Num 3 .* (Num 4 ./ Num 5)
d3 = (Num 4 ./ Num 7) .* (Num 9 ./ Num 3)
d4 = Num 4 ./ Num 8
d5 = (Num 4 .+ Num 5) ./ Num 8

h = (F (Sin (Num 4 .* x)))
c = Num 3 .* x .^ Num 7 .* x .^ Num 2
h1 = c .* h
h2 = h1 ./ (Num 6 .* x .^ Num 4 .+ Num 8 .- x )
h3 = (c ./ (Num 6 .* x .^ Num 4 .+ Num 8 .- x)) .* h




-- testing polyfunc adders things
{-
as = concat $ addCodes (fst (splitAt (length ys `div` 5) ys))
xs = concat $ map unwrapCode $ concat $ addCodes ys'


ys' = (fst (splitAt (length ys `div` 5) ys))
cs' = map unwrapCode ys'
add (c1,p1,x1) (c2,p2,x2) = (c1 .+ c2, p1,x1)
groups = map (map (foldl1 add)) (map gatherArgsPows (transpose cs'))
groups' = transpose $ map (elongate (maximum $ map length groups) (Num 0, Num 0, Num 0)) groups
notAllZero xs = not (all (\x -> x == (Num 0, Num 0, Num 0)) xs)
groups'' = map (map (\(c,p,x) -> (simplify c,p,x))) (filter notAllZero groups')
-}

prep = chisel . distribute . negExplicit
fsc = map codifyPolyFunc (map prep (splitAS (prep e)))
cs' = map unwrapCode fsc
add (c1,p1,x1) (c2,p2,x2) = (c1 .+ c2, p1,x1)
groups = map (map (foldl1 add)) (map gatherArgsPows (transpose cs'))
groups' = transpose $ map (elongate (maximum $ map length groups) (Num 0, Num 0, Num 0)) groups
notAllZero xs = not (all (\x -> x == (Num 0, Num 0, Num 0)) xs)
groups'' = filter notAllZero $ map (map (\(c,p,x) -> (simplify c,p,x))) groups'





-- for testing chisel.

testChisel = testMM' && testMD' && testDM' && testDD'


mm1' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num (-22) .* (F (Sin x)) .* (F (Cos x))
mm2' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num 22)) .* (F (Sin x))
mm3' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num (-22)
mm4' = Num 7 .* Num 8 .* x .^ Num (-22)
mm5' = Num 7 .* x .^ (Num (-22)) .* (F (Sin x)) .* (F (Cos x)) .* Num 8
mm6' = Num 7 .* x .^ (Num (-22)) .* (F (Sin x)) .* x .^ Num 2


testMM1' = (show $ chisel mm1') == "{((7x(8))sin(x)cos(x)) / ((x + 1)^22)}"
testMM2' = (show $ chisel mm2') == "{((7x(8))sin(x)) / ((x + 1)^22)}"
testMM3' = (show $ chisel mm3') == "{(7x(8)) / ((x + 1)^22)}"
testMM4' = (show $ chisel mm4') == "{(7(8)) / (x^22)}"
testMM5' = (show $ chisel mm5') == "{(7sin(x)cos(x)(8)) / (x^22)}"
testMM6' = (show $ chisel mm6') == "{(7sin(x)(x^2)) / (x^22)}"

testMM' = testMM1' && testMM2' && testMM3' && testMM4' && testMM5' && testMM6'


-- div div
dd1' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num 22)))) ./ ((F (Sin x)) .- (F (Cos x)))
dd2' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num 22)))) ./ (F (Sin x))
dd3' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num 22))))
dd4' = (Num 7 .+ x .* Num 8) ./ (x .^ Num (-22))
dd5' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num 22)))) ./ ((F (Sin x)) .* (F (Cos x)) .* x .* Num 2)

testDD1' = (show $ chisel dd1') == "{((7 + x(8))(x^22)) / (sin(x) - cos(x))}"
testDD2' = (show $ chisel dd2') == "{((7 + x(8))(x^22))/sin(x)}"
testDD3' = (show $ chisel dd3') == "(7 + x(8))(x^22)"
testDD4' = (show $ chisel dd4') == "(7 + x(8))(x^22)"
testDD5' = (show $ chisel dd5') == "{((7 + x(8))(x^22)) / (sin(x)cos(x)x(2))}"

testDD' = testDD1' && testDD2' && testDD3' && testDD4' && testDD5'


-- mul div
md1' = Num 7 .* x .* (Num 8 ./ ((x .+ Num 1) .^ Num (-22))) .* (F (Sin x)) .* (F (Cos x) .* (F (Tan x)))
md2' = Num 7 .* x .* (Num 8 ./ ((x .+ Num 1) .^ Num (-22))) .* (F (Sin x)) .* (F (Cos x))
md3' = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ (Neg (Num 22))) .* (F (Sin x))
md4' = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ Num (-22))
md5' = Num 7 .* Num 8 ./ (x .^ (Neg (Num 22)))
md6' = Num 7 ./ (x .^ Num (-22))
md7' = x .^ Num (-22)

testMD1' = (show $ chisel md1') == "(7x(8))sin(x)cos(x)tan(x)((x + 1)^22)"
testMD2' = (show $ chisel md2') == "(7x(8))sin(x)cos(x)((x + 1)^22)"
testMD3' = (show $ chisel md3') == "(7x(8))sin(x)((x + 1)^22)"
testMD4' = (show $ chisel md4') == "7x(8)((x + 1)^22)"
testMD5' = (show $ chisel md5') == "7(8)(x^22)"
testMD6' = (show $ chisel md6') == "7x^22"
testMD7' = (show $ chisel md7') == "{(1) / (x^22)}"

testMD' = testMD1' && testMD2' && testMD3' && testMD4' && testMD5' && testMD6' && testMD7'


-- div mul
dm1' = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num (-22)) ./ ( (F (Sin x)) .* (F (Cos x)))
dm2' = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num (-22)) ./ ( (F (Sin x)) )
dm3' = (Num 8 .* (x .+ Num 1) .^ (Neg (Num 22))) ./ ( (F (Sin x)) .* (F (Cos x)))
dm4' = ((x .+ Num 1) .^ Neg (Num 22)) ./ ( (F (Sin x)) .* (F (Cos x)))
dm5' = ((x .+ Num 1) .^ (Neg (Num 22))) ./ ( (F (Sin x)) )

testDM1' = (show $ chisel dm1') == "{(7x(8)) / (((x + 1)^22)sin(x)cos(x))}"
testDM2' = (show $ chisel dm2') == "{(7x(8)) / (((x + 1)^22)sin(x))}"
testDM3' = (show $ chisel dm3') == "{(8) / (((x + 1)^22)sin(x)cos(x))}"
testDM4' = (show $ chisel dm4') == "{(1) / (((x + 1)^22)sin(x)cos(x))}"
testDM5' = (show $ chisel dm5') == "{(1) / (((x + 1)^22)sin(x))}"

testDM' = testDM1' && testDM2' && testDM3' && testDM4' && testDM5'


-- trivial
mm1 = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num 22 .* (F (Sin x)) .* (F (Cos x))
mm2 = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num 22 .* (F (Sin x))
mm3 = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num 22
mm4 = Num 7 .* Num 8 .* x .^ Num 22
mm1'' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22))) .* (F (Sin x)) .* (F (Cos x))
mm2'' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22))) .* (F (Sin x))
mm3'' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22)))
mm4'' = Num 7 .* Num 8 .* x .^ (Neg (Num (-22)))
---
dd1 = ((Num 7 .+ x .* Num 8) ./ (x .^ Num 22)) ./ ((F (Sin x)) .- (F (Cos x)))
dd2 = ((Num 7 .+ x .* Num 8) ./ (x .^ Num 22)) ./ (F (Sin x))
dd3 = ((Num 7 .+ x .* Num 8) ./ (x .^ Num 22))
dd4 = ((Num 7 .+ x .* Num 8) ./ (x .^ Num 22))
dd1'' = (Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num (-22)))) ./ ((F (Sin x)) .- (F (Cos x)))
dd2'' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num (-22))))) ./ (F (Sin x))
dd3'' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num (-22)))))
dd4'' = (Num 7 ./ (x .^ (Neg (Num (-22)))))
---
md1 = Num 7 .* x .* (Num 8 ./ ((x .+ Num 1) .^ Num 22)) .* (F (Sin x)) .* (F (Cos x))
md2 = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ Num 22) .* (F (Sin x))
md3 = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ Num 22)
md4 = Num 7 .* Num 8 ./ (x .^ Num 22)
md5 = Num 7 ./ (x .^ Num 22)
md1'' = Num 7 .* x .* (Num 8 ./ ((x .+ Num 1) .^ (Neg (Num (-22))))) .* (F (Sin x)) .* (F (Cos x))
md2'' = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ (Neg (Num (-22)))) .* (F (Sin x))
md3'' = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ (Neg (Num (-22))))
md4'' = Num 7 .* Num 8 ./ (x .^ (Neg (Num (-22))))
md5'' = Num 7 ./ (x .^ (Neg (Num (-22))))
---
dm1 = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num 22) ./ ( (F (Sin x)) .* (F (Cos x)))
dm2 = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num 22) ./ ( (F (Sin x)) )
dm3 = (Num 8 .* (x .+ Num 1) .^ Num 22) ./ ( (F (Sin x)) .* (F (Cos x)))
dm4 = ((x .+ Num 1) .^ Num 22) ./ ( (F (Sin x)) .* (F (Cos x)))
dm5 = ((x .+ Num 1) .^ Num 22) ./ ( (F (Sin x)) )
dm1'' = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22)))) ./ ( (F (Sin x)) .* (F (Cos x)))
dm2'' = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22)))) ./ ( (F (Sin x)) )
dm3'' = (Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22)))) ./ ( (F (Sin x)) .* (F (Cos x)))
dm4'' = ((x .+ Num 1) .^ (Neg (Num (-22)))) ./ ( (F (Sin x)) .* (F (Cos x)))
dm5'' = ((x .+ Num 1) .^ (Neg (Num (-22)))) ./ ( (F (Sin x)) )
