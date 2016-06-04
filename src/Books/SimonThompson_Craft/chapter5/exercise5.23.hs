


--duplicate :: a -> Int -> [a]
duplicate     :: String -> Integer -> String
duplicate _ 0 = ""
duplicate s 1 = s
duplicate s n = s ++ duplicate s (n-1)
--take n (repeat s)

duplicate' s n = concat $ take n (repeat s)


duplicate'' ss n
    | n == 0    = ""
    | n == 1    = ss
    | otherwise = [s | c <- [1..n], s <- ss]
    -- help understand how this works compared to: [s | c <- [1..n], s <- ss]