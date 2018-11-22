mapFuns :: [a -> b] -> a -> [b]
mapFuns [] x = []
mapFuns (f:fs) x = f x : mapFuns fs x


mapFuns' :: [a -> b] -> a -> [b]
mapFuns' fs x = map (\f -> f x) fs


mapFuns'' :: [a -> b] -> a -> [b]
mapFuns'' fs x = map ($ x) fs
  -- note ($ x) means the function map applies its function argument (fs) to
  -- the arg x.