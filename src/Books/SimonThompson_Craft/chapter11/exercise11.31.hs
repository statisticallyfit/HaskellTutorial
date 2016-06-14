


{-

Proposition: map f (ys ++ zs) = map f ys ++ map f zs


1. BASE CASE

LEFT
map f ([] ++ zs)
= map f (zs)
= map f zs

RIGHT
map f [] ++ map f zs
= map f zs




3. INDUCTION STEP

LEFT
map f ((y:ys) ++ zs)
= map f (y : (ys ++ zs))
= f y : map f (ys ++ zs)
= f y : map f ys ++ map f zs -- note by induction hypothesis
= map f (y:ys) ++ map f zs


RIGHT
map f (y:ys) ++ map f zs
= f y : map f ys ++ map f zs
= map f (y:ys) ++ map f zs

-}