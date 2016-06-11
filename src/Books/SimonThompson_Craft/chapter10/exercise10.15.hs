import Prelude hiding (unzip, last, init)



unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' xs = (foldr first [] xs, foldr second [] xs)
     where first pair zs = [fst pair] ++ zs
           second pair zs = [snd pair] ++ zs



unzip :: [(a, b)] -> ([a], [b])
unzip xs = foldr (\(a,b) (as,bs) -> (a:as, b:bs)) ([], []) xs

-- note unzip xs = foldr (\(a,b) ~(as,bs) -> (a:as, b:bs)) ([], []) xs
-- the tilde means enforce lazy evaluation. 