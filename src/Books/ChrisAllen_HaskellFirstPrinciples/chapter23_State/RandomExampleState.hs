module RandomExampleState where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random



--newtype State s a = State {runState :: s -> (a,s)}

-- six-sided die
data Die = One | Two | Three | Four | Five | Six deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
                1 -> One
                2 -> Two
                3 -> Three
                4 -> Four
                5 -> Five
                6 -> Six  -- use sparingly (below)
                x -> error $ "intToDie got non 1-6 integer: " ++ show x


-- note
-- state :: Monad m => (s -> (a, s)) -> StateT s m a
rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6) {- stdgen state arg here -}
    return (intToDie n, s) -- here s is state = StdGen

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6)) {- stdgen state arg here -}

rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes = liftA3 (,,) rollDie rollDie rollDie {-stdgen arg is threaded through all-}



-- note: repeat :: a -> [a]
-- answer problem: gives same result since we repeated a single die value and not
-- the state action that produces a die.
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie {-state stdgen arg here -}

-- correct: replicateM :: Monad m => Int -> m a -> m [a]
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie


rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
    where go :: Int -> Int -> StdGen -> Int
          go sum count gen
            | sum >= 20 = count
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen



--- Exercises
--- 1
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit g = go 0 0 g
    where go :: Int -> Int -> StdGen -> Int
          go sum count gen
            | sum >= limit = count
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen

--- 2
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged limit g = go 0 0 [] g
    where go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
          go sum count dieList gen
            | sum >= limit = (count, dieList)
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) (dieList ++ [intToDie die]) nextGen


main = do
    print $ runState rollDie (mkStdGen 0)
    print $ runState rollDie' (mkStdGen 0)
    print $ runState rollDieThreeTimes (mkStdGen 0)
    print $ evalState rollDieThreeTimes (mkStdGen 0)
    print $ evalState rollDieThreeTimes (mkStdGen 1)
    print $ evalState rollDieThreeTimes (mkStdGen 23)
    print $ evalState rollDieThreeTimes (mkStdGen 40)
    print $ evalState rollDieThreeTimes (mkStdGen 5)
    print $ take 30 $ evalState infiniteDie (mkStdGen 0)
    putStrLn ""
    print $ evalState (nDie 30) (mkStdGen 0)
    putStrLn ""
    print $ rollsToGetTwenty (mkStdGen 0)
    -- using IO to get new value for mkStdGen
    (rollsToGetTwenty . mkStdGen ) <$> randomIO
    --- exercise 1
    print $ rollsToGetN 90 (mkStdGen 0)
    print $ rollsCountLogged 70 (mkStdGen 0)
