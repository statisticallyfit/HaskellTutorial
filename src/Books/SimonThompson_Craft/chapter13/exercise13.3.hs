
-- note take pairs and item and returns (snd) of the pair that matches first.
-- note HELP why need Eq b? didn't need this condition before using Maybe.
oneLookupFirst :: (Eq a, Eq b) => [(a,b)] -> a -> Maybe b
oneLookupFirst ps first = second
    where matchingPairs = filter (\(x, y) -> x == first) ps
          second = if matchingPairs == [] then Nothing
                   else Just (snd $ head matchingPairs)

oneLookupSecond :: (Eq a, Eq b) => [(a,b)] -> b -> Maybe a
oneLookupSecond ps second = first
    where matchingPairs = filter (\(x,y) -> y == second) ps
          first = if matchingPairs == [] then Nothing
                  else Just (fst $ head matchingPairs )