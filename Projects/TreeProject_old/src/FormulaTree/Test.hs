n = 4
(g, h) = opFunc "+" (+ 1) n
opFunc op func n
    | op == "+" = (((func n) +), (\x -> x + (func n)))
    | op == "-" = (((func n) -), (\x -> x - (func n)))
    | op == "*" = (((func n) *), (\x -> x * (func n)))
    | op == "/" = (((func n) `div`), (\x -> x `div` (func n)))
    | op == "^" = (((func n) ^), (\x -> x ^ (func n)))








-- For testing chisel function

-- mul mul
mm1 = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num 22 .* (F (Sin x)) .* (F (Cos x))
mm2 = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num 22 .* (F (Sin x))
mm3 = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num 22
mm4 = Num 7 .* Num 8 .* x .^ Num 22
mm1' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num (-22) .* (F (Sin x)) .* (F (Cos x))
mm2' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num 22)) .* (F (Sin x))
mm3' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num (-22)
mm4' = Num 7 .* Num 8 .* x .^ Num (-22)
mm1'' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22))) .* (F (Sin x)) .* (F (Cos x))
mm2'' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22))) .* (F (Sin x))
mm3'' = Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22)))
mm4'' = Num 7 .* Num 8 .* x .^ (Neg (Num (-22)))
-- div div
dd1 = ((Num 7 .+ x .* Num 8) ./ (x .^ Num 22)) ./ ((F (Sin x)) .- (F (Cos x)))
dd2 = ((Num 7 .+ x .* Num 8) ./ (x .^ Num 22)) ./ (F (Sin x))
dd3 = ((Num 7 .+ x .* Num 8) ./ (x .^ Num 22))
dd4 = ((Num 7 .+ x .* Num 8) ./ (x .^ Num 22))
dd1' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num 22)))) ./ ((F (Sin x)) .- (F (Cos x)))
dd2' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num 22)))) ./ (F (Sin x))
dd3' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num 22))))
dd4' = (Num 7 .+ x .* Num 8) ./ (x .^ Num (-22))
dd5' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num 22)))) ./ ((F (Sin x)) .* (F (Cos x)) .* x .* Num 2)
dd1'' = (Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num (-22)))) ./ ((F (Sin x)) .- (F (Cos x)))
dd2'' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num (-22))))) ./ (F (Sin x))
dd3'' = ((Num 7 .+ x .* Num 8) ./ (x .^ (Neg (Num (-22)))))
dd4'' = (Num 7 ./ (x .^ (Neg (Num (-22)))))
-- mul div
md1 = Num 7 .* x .* (Num 8 ./ ((x .+ Num 1) .^ Num 22)) .* (F (Sin x)) .* (F (Cos x))
md2 = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ Num 22) .* (F (Sin x))
md3 = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ Num 22)
md4 = Num 7 .* Num 8 ./ (x .^ Num 22)
md5 = Num 7 ./ (x .^ Num 22)
md1' = Num 7 .* x .* (Num 8 ./ ((x .+ Num 1) .^ Num (-22))) .* (F (Sin x)) .* (F (Cos x) .* (F (Tan x)))
md2' = Num 7 .* x .* (Num 8 ./ ((x .+ Num 1) .^ Num (-22))) .* (F (Sin x)) .* (F (Cos x))
md3' = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ (Neg (Num 22))) .* (F (Sin x))
md4' = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ Num (-22))
md5' = Num 7 .* Num 8 ./ (x .^ (Neg (Num 22)))
md6' = Num 7 ./ (x .^ Num (-22))
md7' = x .^ Num (-22)
md1'' = Num 7 .* x .* (Num 8 ./ ((x .+ Num 1) .^ (Neg (Num (-22))))) .* (F (Sin x)) .* (F (Cos x))
md2'' = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ (Neg (Num (-22)))) .* (F (Sin x))
md3'' = Num 7 .* x .* Num 8 ./ ((x .+ Num 1) .^ (Neg (Num (-22))))
md4'' = Num 7 .* Num 8 ./ (x .^ (Neg (Num (-22))))
md5'' = Num 7 ./ (x .^ (Neg (Num (-22))))
-- div mul
dm1 = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num 22) ./ ( (F (Sin x)) .* (F (Cos x)))
dm2 = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num 22) ./ ( (F (Sin x)) )
dm3 = (Num 8 .* (x .+ Num 1) .^ Num 22) ./ ( (F (Sin x)) .* (F (Cos x)))
dm4 = ((x .+ Num 1) .^ Num 22) ./ ( (F (Sin x)) .* (F (Cos x)))
dm5 = ((x .+ Num 1) .^ Num 22) ./ ( (F (Sin x)) )
dm1' = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num (-22)) ./ ( (F (Sin x)) .* (F (Cos x)))
dm2' = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ Num (-22)) ./ ( (F (Sin x)) )
dm3' = (Num 8 .* (x .+ Num 1) .^ (Neg (Num 22))) ./ ( (F (Sin x)) .* (F (Cos x)))
dm4' = ((x .+ Num 1) .^ Neg (Num 22)) ./ ( (F (Sin x)) .* (F (Cos x)))
dm5' = ((x .+ Num 1) .^ (Neg (Num 22))) ./ ( (F (Sin x)) )
dm1'' = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22)))) ./ ( (F (Sin x)) .* (F (Cos x)))
dm2'' = (Num 7 .* x .* Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22)))) ./ ( (F (Sin x)) )
dm3'' = (Num 8 .* (x .+ Num 1) .^ (Neg (Num (-22)))) ./ ( (F (Sin x)) .* (F (Cos x)))
dm4'' = ((x .+ Num 1) .^ (Neg (Num (-22)))) ./ ( (F (Sin x)) .* (F (Cos x)))
dm5'' = ((x .+ Num 1) .^ (Neg (Num (-22)))) ./ ( (F (Sin x)) )

