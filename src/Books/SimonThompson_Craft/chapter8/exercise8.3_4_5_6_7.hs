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


--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

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
    | a == b    = elimOneStrategy [a] -- note must be list because of strategy's type
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

-- note takes a list of moves and returns their frequencies
-- returns a list of length 3 which holds the move and its frequency
-- postcondition (rockCount, paperCount, scissorsCount)
frequencies :: [Move] -> (Int, Int, Int) -> (Int, Int, Int)
frequencies [] (rc, pc, sc) = (rc, pc, sc)
frequencies (m:ms) (rc, pc, sc)
    | m == Rock      = frequencies ms (rc + 1, pc, sc)
    | m == Paper     = frequencies ms (rc, pc + 1, sc)
    | otherwise      = frequencies ms (rc, pc, sc + 1)


-- note: assume the opponent is about to choose his least frequent move. Now
-- choose the move that will either win or draw in response to that one.
-- So if least Frequent is Scissors, then choose the best of Rock/Paper. Paper beats
-- Rock so choose Rock. That way
-- > if she chooses Rock, you beat her
-- > if she cooses Paper, there is a draw
leastFrequentStrategy :: Strategy
leastFrequentStrategy moves = elimOneStrategy [leastFreqMove]
        where (rc,pc,sc) = frequencies moves (0,0,0)
              leastFreqCount = minimum [rc, pc, sc]
              [rBool, pBool, sBool] = fmap (== leastFreqCount) [rc, pc, sc]
              leastFreqMove | rBool = Rock
                            | pBool = Paper
                            | sBool = Scissors



-- exercise 6 --------------------------------------------------------------------------

-- note: assume the opponent is about to choose his MOST frequent move. Now
-- choose the move that will either win or draw in response to that one.
mostFrequentStrategy :: Strategy
mostFrequentStrategy moves = elimOneStrategy [mostFreqMove]
        where (rc,pc,sc) = frequencies moves (0,0,0)
              mostFreqCount = maximum [rc, pc, sc]
              [rBool, pBool, sBool] = fmap (== mostFreqCount) [rc, pc, sc]
              mostFreqMove | rBool = Rock
                           | pBool = Paper
                           | sBool = Scissors


ms = [Rock, Rock, Rock, Rock, Paper, Paper, Scissors, Paper, Paper, Scissors, Paper,
      Rock, Scissors, Rock, Scissors, Rock, Rock, Scissors, Paper, Paper, Rock,
      Rock, Rock, Rock, Rock, Rock, Paper, Scissors]

f1 = leastFrequentStrategy ms -- will be Paper since leastFreq is Scissors
f2 = mostFrequentStrategy ms -- will be Scissors since mostfreq is Rock





-- exercise 7 --------------------------------------------------------------------------
-- Had help here now i understand answer. But how to test these since none of the
-- strategies return list of moves?
alternate :: Strategy -> Strategy -> Strategy
alternate primaryStrategy secondaryStrategy moves
    | even (length moves) = primaryStrategy moves
    | otherwise           = secondaryStrategy moves


alternate' :: Strategy -> Strategy -> Strategy
alternate' firstStrategy alternateStrategy moves
    = map ($ moves) [firstStrategy, alternateStrategy]
                            !! (length moves `rem` 2)




-- exercise 8 --------------------------------------------------------------------------
-- HELP HELP HELP TODO IMPORTANT