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

-- note todo http://stackoverflow.com/questions/8416365/generate-a-random-integer-in-a-range-in-haskell

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












-- exercise 3 --------------------------------------------------------------------------

beatLastMove :: Strategy
beatLastMove []     = Rock -- arbitrary choice
beatLastMove (m:ms) = beat m

loseLastMove :: Strategy
loseLastMove []     = Scissors
loseLastMove (m:ms) = lose m


-- exercise 4 --------------------------------------------------------------------------

{-
note :
- when opponent plays two same moves in a row, then expect next move is different.
Example: Rock, Rock then next one is either Paper or Scissors. My choice should be
Scissors because we draw if she has Scissors and I win if she has Paper.

- when opponent does not play two same moves in a row, give random choice
-}
lastTwo :: Strategy
lastTwo (a:b:cs)
    | a == b    = elimOneStrategy (a:b:cs)
    | otherwise = randomStrategy (a:b:cs)

-- precondition assume last two moves are the same so that m1 == m2. Note that it
-- doesn't matter what value the m3 was.
-- Called the elimTwo because one move is eliminated from oppponents moves so now
-- you know 2 can possible occurs. Choose the winning move from those two.
elimOneStrategy :: Strategy
elimOneStrategy (m:ms)
    | m == Rock  = Scissors
    | m == Paper = Rock
    | otherwise  = Paper


t1 = lastTwo [Rock, Rock, Paper] == Scissors
t2 = lastTwo [Paper, Paper, Rock] == Rock
t3 = lastTwo [Scissors, Scissors, Paper] == Paper





-- exercise 5 --------------------------------------------------------------------------


-- note: assume the opponent is about to choose his least frequent move. Now
-- choose the move that will either win or draw in response to that one.
leastFrequentStrategy :: Strategy
leastFrequentStrategy 


-- note: assume the opponent is about to choose his MOST frequent move. Now
-- choose the move that will either win or draw in response to that one.






