import System.Random


{-
1. StdGen
2. mkStdGen :: Int -> StdGen
3. next :: g -> (Int, g) , where g is value of type StdGen.
4. random :: (RandomGen g, Random a) => g -> (a, g)

example

Prelude> :t mkStdGen 0
mkStdGen 0 :: StdGen
Prelude> let sg = mkStdGen 0
Prelude> :t next sg
next sg :: (Int, StdGen)
Prelude> next sg
(2147482884,40014 40692)
Prelude> next sg
(2147482884,40014 40692)

-}