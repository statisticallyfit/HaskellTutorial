import System.Random
--import Control.Monad.State

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

note see how lack of state means next sg gives same output over and over.

let newSg = snd (next sg)
*Main> next sg
(2147482884,40014 40692)
*Main> newSg
40014 40692
*Main> :t newSg
newSg :: StdGen
*Main> next newSg
(2092764894,1601120196 1655838864)
*Main> next (snd (next newSg))
(1390461064,1346387765 2103410263)
*Main> next (snd it)
(715295839,439883729 1872071452)
*Main> next (snd it)
(79337801,732249858 652912057)
*Main> next (snd it)
(347273588,2127568003 1780294415)
*Main> next (snd it)
(1427314282,1962667596 535353314)

Main> random newSg
(138890298504988632,439883729 1872071452)
*Main> random newSg :: (Double , StdGen )
(0.41992072972993366,439883729 1872071452)

*Main> :t randomR
randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
*Main> randomR (0,3) newSg :: (Int, StdGen )
(1,1601120196 1655838864)
*Main> randomR (0,3) newSg :: (Int, StdGen )
(1,1601120196 1655838864)

-}


-- note is isomorphic: means there isa way to go from the newtype to the thing it
-- wraps and back again without losing information.
newtype State s a = State {runState :: s -> (a,s)}

-- State :: (s -> (a,s)) -> State s a
-- runState :: State s a -> s -> (a,s)

-- examples
-- randomR :: (...) => (a, a) -> g      -> (a, g)
-- random :: (Random a) =>       StdGen -> (a, StdGen)
-- equals State:                 s      -> (a, s)

