iter :: Integer -> (a -> a) -> a -> a
iter n f
    | n > 0     = f . iter (n - 1) f
    | otherwise = id

eval2 = iter 10 succs 12



{-
NOTE

let double x = 2 * x

iter 3 double 1
= double ( iter 2 double 1)
= double (double (iter 1 double 1))
= double (double (double (iter 0 double 1)))
= double (double (double (id 1)))
= double (double (double 1))
= double (double 2)
= double 4
= 8

-}