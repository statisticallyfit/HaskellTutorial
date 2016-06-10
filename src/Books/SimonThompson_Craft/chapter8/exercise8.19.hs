

copy :: IO()
copy = do line <- getLine
          let whileCopy = do
                             if (line == "")
                             then (return ())
                             else do putStrLn line
                                     line <- getLine
                                     whileCopy
          whileCopy



{-
note problem: there are two recursions
hi
hi
a
hi
b
hi
c
hi
d
hi
e
hi
f
hi
g
hi

-}


