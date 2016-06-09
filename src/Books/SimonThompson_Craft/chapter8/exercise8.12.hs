


putNtimes :: Integer -> String -> IO()
putNtimes n str = do
                    let lines = concat $ take (fromIntegral n) $ repeat (str ++ "\n")
                    putStr lines

main = do
    putNtimes 30 "azure skies"