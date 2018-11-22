{-
NOTE use these linsk to help
https://github.com/kairos1729/hutton/blob/master/chapter9exercises.hs
https://github.com/yellowflash/Hutton-Euler/blob/master/Hutton8.hs
-}


{-
note
The action getCh that reads a character without echoing is not part of the
standard prelude, but is provided as an extension by Hugs and can be made
available in any script by including the following special line:
primitive getCh :: IO Char
-}

-- note: The action getCh that reads a character without echoing is not part of the
--         standard prelude, but is provided as an extension by Hugs and can be made
--         available in any script by including the following special line:
--         primitive getCh :: IO Char

{-
NOTE

type IO a = World → (a, World)


getChar :: IO Char
getChar = do x ← getCh
             putChar x
             return x


putChar :: Char → IO ()
putChar c = ···

return :: a → IO a
return v = λworld → (v, world)


(>>=) :: IO a → (a → IO b) → IO b
f >>= g = λworld → case f world of
                    (v, world') → g v world'


-- HELP understand better
getLine :: IO String
getLine = do x ← getChar
             if x == ’\n’ then
                return []
             else
                do xs ← getLine
                   return (x : xs)    HELP

putStr :: String → IO ()
putStr [] = return ()
putStr (x : xs) = do putChar x
                     putStr xs

putStr' xs = seqn [putChar x | x <- xs]


putStrLn :: String → IO ()
putStrLn xs = do putStr xs
                 putChar ’\n’


-}


strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"








-- position
type Pos = (Int, Int)


-- moves cursor to a given position
goto :: Pos -> IO()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- displays string at given position
writeat :: Pos -> String -> IO()
writeat p xs = do goto p
                  putStr xs



-- performs list of actions in sequence, discarding results returning no result.
seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as


---------------------------------------------------------------------------------------------
-- HERE need parser NOTE


box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]


buttons :: [Char]
buttons = standard ++ extra
          where
            standard = "qcd=123+456-789*0()/"
            extra = "QCD \ESC\BS\DEL\n"


showBox :: IO ()
showBox = seqn [writeat (1, y) xs | (y, xs) <- zip [1..13] box ]


display :: String -> IO()
display xs = do writeat (3,2) "                   "
                writeat (3,2) (reverse (take 13 (reverse xs)))

{-

calc :: String -> IO()
calc xs = do display xs
             c <- getCh  -- HELP find way to load this in here...
             if elem c buttons then
                process c xs
             else
                do beep
                    calc xs -- if not a button, sound beep then continue with string.



process :: Char -> String -> IO()
process c xs
    | elem c "qQ\ESC"    = quit
    | elem c "dD\BS\DEL" = delete xs
    | elem c "=\n"       = eval xs
    | elem c "cC"        = clear
    | otherwise          = press c xs


-- now the five possible actions
quit :: IO()
quit = goto (1,14) -- moves cursor below calculator box and terminates

delete :: String -> IO()
delete "" = calc ""        -- if empty does nothing
delete xs = calc (init xs)  -- removes last character if not empty string

eval :: String -> IO()
eval xs = case parse expr xs of
            [(n, "")] -> calc (show n)
            _         -> do beep
                            calc xs

clear :: IO()
clear = calc ""

-- any other character is append to the end of current string
press :: Char -> String -> IO()
press c xs = calc (xs ++ [c])

-- clears screen, displays box, stars with empty display
run :: IO()
run = do cls
         showBox
         clear
-}

main = do
    seqn [writeat (50,70) "HEY!", writeat (70,90) "SUP?"]