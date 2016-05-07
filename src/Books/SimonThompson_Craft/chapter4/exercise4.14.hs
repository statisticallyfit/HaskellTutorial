
-- raise 2 to power n

toPower :: Int -> Int
toPower p
    | p < 0     = error "No negative expoonent allowed"
    | p == 0    = 1
    | even p    = (toPower (p `div` 2)) * (toPower (p `div` 2))
    | otherwise = 2 * (toPower (p `div` 2)) * (toPower (p `div` 2)) -- odd


main = do
    print $ toPower 4
    print $ toPower 8
    print $ toPower 5