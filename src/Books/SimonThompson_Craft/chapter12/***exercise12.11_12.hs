import Data.Time
import System.IO.Unsafe

data Move = Rock | Paper | Scissors deriving (Eq, Show)
type Strategy = [Move] -> Move


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

randomStrategy :: Strategy
randomStrategy _ = convertIntegerToMove (randInt 3)
------------------------------------------------------------------------------------

-- strategies
echo :: Strategy
echo (latest : rest) = latest
echo []              = Rock

beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat _ = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose _ = Paper

beatLastMove :: Strategy
beatLastMove []     = Rock -- arbitrary choice
beatLastMove (m:ms) = beat m

loseLastMove :: Strategy
loseLastMove []     = Scissors
loseLastMove (m:ms) = lose m

frequencies :: [Move] -> (Int, Int, Int) -> (Int, Int, Int)
frequencies [] (rc, pc, sc) = (rc, pc, sc)
frequencies (m:ms) (rc, pc, sc)
    | m == Rock      = frequencies ms (rc + 1, pc, sc)
    | m == Paper     = frequencies ms (rc, pc + 1, sc)
    | otherwise      = frequencies ms (rc, pc, sc + 1)


------------------------------------------------------------------------------------


-- exercise 11 ---------------------------------------------------------------------
majority :: [Strategy] -> Strategy
majority ss moves = getMostFrequentMove calculatedMoves
    where calculatedMoves = map ($ moves) ss



-- note in case of a tie, get a random move
getMostFrequentMove :: [Move] -> Move
getMostFrequentMove ms = if allTrue then (randomStrategy ms) else mostFreq
    where (rc,pc,sc) = frequencies ms (0,0,0)
          mostFreqCount = maximum [rc, pc, sc]
          [rBool, pBool, sBool] = fmap (== mostFreqCount) [rc, pc, sc]
          bools = [rBool, pBool, sBool]
          allTrue = all (== True) bools
          mostFreq | rBool = Rock
                   | pBool = Paper
                   | sBool = Scissors



-- exercise 12 ---------------------------------------------------------------------
-- note run all ss (strategies) on list of opponent moves and compare resultList
-- with opponentMoves to decide which strategy beats opponent's moves the most times.
-- note process:
-- 1. compare opponent moves with result moves using isWin to and say true when
-- the move in result list beat the opponent's move.
-- 2. get indexes of all trues
-- 3. get all strategies at those indexes
-- 4. get majority strategy
-- alternate solution: (because we can't compare functions)
-- 1. compare opp and result list
-- 2. get index of first true
-- 3. get strategy with that index
{-
train :: [Moves] -> [Strategy] -> Strategy
train opponentMoves ss
    where resultMoves = map ($ opponentMoves) ss
-}
-- HELP HELP HELP - how to measure wins: zipWith isWin resultMoves oppMoves
-- problem is - most strategies are define to just beat the head of the moves
-- and won't return a list. How to use this definition for this function?

beatAll :: [Move] -> [Move]
beatAll = map beat

loseAll :: [Move] -> [Move]
loseAll = map lose

echoAll :: [Move] -> [Move]
echoAll ms = ms


isWin :: Move -> Move -> Bool
isWin Paper Rock = True
isWin Rock Scissors = True
isWin Scissors Paper = True
isWin _ _ = False