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

------------------------------------------------------------------------------------


-- exercise 9
sTossList :: [Strategy] -> Strategy
sTossList ss moves
    | length ss == 0 = error "No strategies given"
    | otherwise      = map ($ moves) ss !! (fromIntegral (randInt (toInteger (length ss))))