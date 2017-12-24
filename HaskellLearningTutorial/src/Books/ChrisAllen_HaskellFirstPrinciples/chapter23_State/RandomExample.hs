module RandomExample where

import System.Random

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



rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
    -- this will produce the same results every time since it is pure
    -- or free of effects.
    let s = mkStdGen 0
        (d1, s1) = randomR (1, 6) s
        (d2, s2) = randomR (1, 6) s1
        (d3, s3) = randomR (1, 6) s2
    (intToDie d1, intToDie d2, intToDie d3)
