-- Exercise 1 ---------------------------------------------------------------------

putStrLn' str = do
                    putStr str
                    putChar '\n'

{-}main = do
    putStrLn' "First line"
    putStrLn' "Second line"
-}


-- Exercise 2,3 ---------------------------------------------------------------------
putQStrLn str = do
        putChar '"'
        putStr str
        putChar '"'
        putChar '\n'

main = do
    putStrLn "Enter text: "
    str <- getLine
    putQStrLn str

