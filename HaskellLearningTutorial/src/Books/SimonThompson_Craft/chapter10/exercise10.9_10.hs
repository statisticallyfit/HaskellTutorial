

iter :: Integer -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = f (iter (n-1) f x)


double x = 2 * x


twoToPower :: Integer -> Integer
twoToPower n = iter n  double 1