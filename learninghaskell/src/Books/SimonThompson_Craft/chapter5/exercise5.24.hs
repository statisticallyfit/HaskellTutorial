
-- note the x <- [1..} part is used as a counter and as it goes, the spaces
-- are produced.
pushRight :: String -> Int -> String
pushRight xs lineLength = [' ' | x <- [1 .. lineLength - n]] ++ xs
                          where n = length xs


pushRight' :: String -> Int -> String
pushRight' xs lineLength = (replicate n ' ') ++ xs
                            where n = lineLength - (length xs)
main = do
    print $ pushRight "crocodile" 12
    print $ pushRight' "crocodile" 12