n = 4
(g, h) = opFunc "+" (+ 1) n
opFunc op func n
    | op == "+" = (((func n) +), (\x -> x + (func n)))
    | op == "-" = (((func n) -), (\x -> x - (func n)))
    | op == "*" = (((func n) *), (\x -> x * (func n)))
    | op == "/" = (((func n) `div`), (\x -> x `div` (func n)))
    | op == "^" = (((func n) ^), (\x -> x ^ (func n)))