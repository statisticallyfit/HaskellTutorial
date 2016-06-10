import Data.Time
import System.IO.Unsafe
import Data.Text (pack, unpack, justifyLeft)


data Move = Rock | Paper | Scissors deriving (Eq, Show)

data Result = Win | Lose | Draw deriving (Eq, Show)

type Tournament = ([Move], [Move]) -- moves made by two players

type Strategy = [Move] -> Move -- note next move depends on previous opponent moves.



outcome :: Move -> Move -> Result
outcome Rock Scissors     = Win
outcome Paper Rock        = Win
outcome Scissors Paper    = Win
outcome Rock Paper        = Lose
outcome Paper Scissors    = Lose
outcome Scissors Rock     = Lose
outcome Rock Rock         = Draw
outcome Paper Paper       = Draw
outcome Scissors Scissors = Draw

-- OFFICIAL STRATEGIES --------------------------------------------------------------



randomInt :: Integer -> IO Integer -- note uses the IO monad
randomInt n = do time <- getCurrentTime
                 return ((`rem` n) $ read $ take 6 $
                                formatTime defaultTimeLocale "%q" time)

randInt :: Integer -> Integer
randInt = unsafePerformIO . randomInt

convertIntegerToMove :: Integer -> Move
convertIntegerToMove 0 = Rock
convertIntegerToMove 1 = Paper
convertIntegerToMove 2 = Scissors


frequencies :: [Move] -> (Int, Int, Int) -> (Int, Int, Int)
frequencies [] (rc, pc, sc) = (rc, pc, sc)
frequencies (m:ms) (rc, pc, sc)
    | m == Rock      = frequencies ms (rc + 1, pc, sc)
    | m == Paper     = frequencies ms (rc, pc + 1, sc)
    | otherwise      = frequencies ms (rc, pc, sc + 1)

beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat _ = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose _ = Paper
------------------------------------------------------------------------------------------
cycle :: Strategy
cycle moves = case (length moves) `rem` 3 of
                0 -> Rock
                1 -> Paper
                2 -> Scissors


echo :: Strategy
echo (latest : rest) = latest
echo []              = Rock



beatLastMove :: Strategy
beatLastMove []     = Rock -- arbitrary choice
beatLastMove (m:ms) = beat m

loseLastMove :: Strategy
loseLastMove []     = Scissors
loseLastMove (m:ms) = lose m


lastTwo :: Strategy
lastTwo (a:b:cs)
    | a == b    = elimOneStrategy [a]
    | otherwise = randomStrategy (a:b:cs)

elimOneStrategy :: Strategy
elimOneStrategy (m:ms)
    | m == Rock  = Scissors
    | m == Paper = Rock
    | otherwise  = Paper

randomStrategy :: Strategy
randomStrategy _ = convertIntegerToMove (randInt 3)



leastFrequentStrategy :: Strategy
leastFrequentStrategy moves = elimOneStrategy [leastFreqMove]
        where (rc,pc,sc) = frequencies moves (0,0,0)
              leastFreqCount = minimum [rc, pc, sc]
              [rBool, pBool, sBool] = fmap (== leastFreqCount) [rc, pc, sc]
              leastFreqMove | rBool = Rock
                            | pBool = Paper
                            | sBool = Scissors

mostFrequentStrategy :: Strategy
mostFrequentStrategy moves = elimOneStrategy [mostFreqMove]
        where (rc,pc,sc) = frequencies moves (0,0,0)
              mostFreqCount = maximum [rc, pc, sc]
              [rBool, pBool, sBool] = fmap (== mostFreqCount) [rc, pc, sc]
              mostFreqMove | rBool = Rock
                           | pBool = Paper
                           | sBool = Scissors


alternate :: Strategy -> Strategy -> Strategy
alternate primaryStrategy secondaryStrategy moves
    | even (length moves) = primaryStrategy moves
    | otherwise           = secondaryStrategy moves







-- MAKING STRATEGIES -----------------------------------------------------------------

-- 1 constant
rock, paper, scissors :: Strategy
rock _ = Rock
paper _ = Paper
scissors _ = Scissors

{-

-- 2 cycle through all three possibilities
cycle :: Strategy
cycle moves = case (length moves) `rem` 3 of
                0 -> Rock
                1 -> Paper
                2 -> Scissors


-- 3 random
randomStrategy :: Strategy
randomStrategy _ = convertIntegerToMove (randInt 3)

randomInt :: Integer -> IO Integer -- note uses the IO monad
randomInt n = do time <- getCurrentTime
                 return ((`rem` n) $ read $ take 6 $
                                formatTime defaultTimeLocale "%q" time)

randInt :: Integer -> Integer
randInt = unsafePerformIO . randomInt

convertIntegerToMove :: Integer -> Move
convertIntegerToMove 0 = Rock
convertIntegerToMove 1 = Paper
convertIntegerToMove 2 = Scissors


-- 4 echo last move

-- precondition: assume latest move (newest) is first in list
-- so [Rock, Rock, Paper] says that Paper was first.
echo :: Strategy
echo (latest : rest) = latest
echo []              = Rock -- note when not eprevious moves do Rock



-}







-- 8.4 EXAMPLES  OF DO NOTATION --------------------------------------------------------

reverseTwoLines :: IO()
reverseTwoLines = do line1 <- getLine
                     line2 <- getLine
                     let rev1 = reverse line1
                     let rev2 = reverse line2 -- since no longer io, cannot use do <-
                     putStrLn rev2
                     putStrLn rev1

getInt :: IO Integer
getInt = do line <- getLine
            return (read line :: Integer)







-- 8.5 LOOPS AND RECURSION -----------------------------------------------------------

-- note called tail recursive since only recursive part is last statement.
copy :: IO()
copy = do line <- getLine
          putStrLn line
          copy


copyN :: Integer -> IO()
copyN n = if n <= 0
          then return ()
          else do line <- getLine
                  putStrLn line
                  copyN (n - 1)

-- note copy lines until empty line is encountered
copyEmpty :: IO()
copyEmpty = do line <- getLine
               if line == ""
               then return ()
               else do putStrLn line
                       copyEmpty



-- note count num lines you have copied. n is accumulator, start it with 0.
copyCount :: Integer -> IO()
copyCount n = do line <- getLine
                 if line == ""
                 then putStrLn (show n ++ " lines copied.")
                 else do putStrLn line
                         copyCount (n + 1)







-- 8.6 ROCK PAPER SCISSORS - PLAYING THE GAME.


t1, t2, t3, t4, t5 :: Tournament
t1 = ([Rock, Rock,Paper],[Scissors,Paper,Rock])
t2 = ([Scissors, Rock,Paper],[Scissors,Rock,Paper])
t3 = ([Paper, Rock, Paper],[Scissors, Paper, Rock])
t4 = ([Scissors, Paper, Rock],[Rock, Scissors, Paper])
t5 = ([Rock, Scissors, Paper], [Scissors, Paper, Rock])
tLong = (fst t1 ++ fst t2 ++ fst t3, snd t1 ++ snd t2 ++ snd t3 )




showTournament :: Tournament -> String
showTournament (myMoves, yourMoves) = "Mine:" ++ topSpace ++ " | Yours:\n" ++
    take 19 (repeat '-') ++ "\n" ++ -- note len of above top line + 2 for Scissors (rs)
    concat [(justifyAndLine m) ++ show y ++ "\n" | (m,y) <- zip myMoves yourMoves]
    where justifyAndLine m = take (lenM m - 2) (justify m) ++ "| "
          lenM m = length (justify m)
          justify m = unpack (justifyLeft n ' ' (pack (show m)))
          n = length space + 3 -- length (space ++ "    ") -- + a space of 4
          space = take lenScis (repeat ' ')  -- take 16 (repeat ' ')
          topSpace = take (lenScis - (length "Mine:")) (repeat ' ')
          lenScis = length (show Scissors)

-- note assume most recent is first so show that last in the tournament box.
printTournament :: Tournament -> IO()
printTournament (myMoves, yourMoves) = putStrLn $ showTournament (reverse myMoves,
                                                                  reverse yourMoves)




convertCharToMove :: Char -> Move
convertCharToMove ch | ch == 'r' = Rock
                     | ch == 's' = Scissors
                     | ch == 'p' = Paper


play :: Strategy -> IO()
play strategy = playInteractive strategy ([], [])

-- note the @ matches the constructor 't' to what is inside (mine, yours) so now
-- t is holding mine and yours lists.
-- note assume computer is responding to player.
playInteractive :: Strategy -> Tournament -> IO()
playInteractive strategy t@(mine, yours) =
    do putStr "Input char >  "
       ch <- getChar
       if not (ch `elem` "rpsRPS")
       then do putStrLn "\n"
               printTournament t
       else do let yourMove = convertCharToMove ch
               let next = strategy (yourMove:yours) --- note since yours is opponent for computer.
               putStrLn ("\nYou played: " ++ [ch] ++ " now I play: " ++ show next)
               playInteractive strategy (next:mine, yourMove:yours)


{-
NOTE original by book - wasn't responding direclty to most recent play of opponent (ME)

       else do let next = strategy yours --- note since yours is opponent for computer.
               putStrLn ("\nYou played: " ++ [ch] ++ " now I play: " ++ show next)
               let yourMove = convertCharToMove ch
               playInteractive strategy (next:mine, yourMove:yours)
-}




