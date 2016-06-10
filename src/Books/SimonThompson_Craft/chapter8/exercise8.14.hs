import Data.List (words)


--wc :: IO()
wc = wc' (0,0,0)
    where wc' (cct, wct, lct) = do line <- getLine
                                   if line /= ""
                                   then (
                                        wc' (cct + (sum $ fmap length (words line)),
                                        wct + length (words line),
                                        lct + 1)
                                        )
                                   else (
                                        do putStrLn ("char count: " ++ (show cct))
                                           putStrLn ("word count: " ++ (show wct))
                                           putStrLn ("line count: " ++ (show lct))
                                        )




main = wc