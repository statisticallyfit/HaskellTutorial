import Data.Time
import System.IO.Unsafe

data Move = Rock | Paper | Scissors deriving (Eq, Show)

data Result = Win | Lose | Draw deriving (Eq, Show)

type Tournament = ([Move], [Move]) -- moves made by two players

type Strategy = [Move] -> Move -- note next move depends on previous opponent moves.


---------------------------------------------------------------------------------------


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



beatStrategy :: Strategy -> Strategy
beatStrategy opponentStrat moves = beat (opponentStrat moves)