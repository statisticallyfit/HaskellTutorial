import Data.Time
import System.IO.Unsafe


data Move = Rock | Paper | Scissors deriving (Eq, Show)

data Result = Win | Lose | Draw deriving (Eq, Show)

type Tournament = ([Move], [Move]) -- moves made by two players

type Strategy = [Move] -> Move -- note next move depends on previous opponent moves.



beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat _ = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose _ = Paper

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




-- STRATEGIES ------------------------------------------------------------------------

-- 1 constant
rock, paper, scissors :: Strategy
rock _ = Rock
paper _ = Paper
scissors _ = Scissors


-- 2 cycle through all three possibilities
cycle :: Strategy
cycle moves = case (length moves) `rem` 3 of
                0 -> Rock
                1 -> Paper
                2 -> Scissors


-- 3 random
randomStrategy :: Strategy
randomStrategy _ = convertToMove (randInt 3)

randomInt :: Integer -> IO Integer -- note uses the IO monad
randomInt n = do time <- getCurrentTime
                 return ((`rem` n) $ read $ take 6 $
                                formatTime defaultTimeLocale "%q" time)

randInt :: Integer -> Integer
randInt = unsafePerformIO . randomInt

convertToMove :: Integer -> Move
convertToMove 0 = Rock
convertToMove 1 = Paper
convertToMove 2 = Scissors


-- 4 echo last move

-- precondition: assume latest move (newest) is first in list
-- so [Rock, Rock, Paper] says that Paper was first.
echo :: Strategy
echo (latest : rest) = latest
echo []              = Rock -- note when not eprevious moves do Rock