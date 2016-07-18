module FizzBuzz where


import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL



--newtype State s a = State { runState :: s -> (a, s) }

{-
fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Fizz"
           | n `mod` 3 == 0 = "Buzz"
           | otherwise = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list =
    let dlist = execState (mapM_ addResult list) DL.empty
    in DL.apply dlist [] -- convert back to normal list

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    -- snoc appends to the end unlike cons which adds to front
    put (DL.snoc xs result)



main :: IO()
main = mapM_ putStrLn $ fizzBuzzList [1.. 100]
-- note slow solution because of reverse:
-- main = mapM_ putStrLn $ reverse $ fizzBuzzList [1 .. 100]-}



-- note cleaning up to let DL's Foldable instance convert to list before folding.

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Fizz"
           | n `mod` 3 == 0 = "Buzz"
           | otherwise = show n

fizzBuzzList :: [Integer] -> DL.DList String
fizzBuzzList list = execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    -- snoc appends to the end unlike cons which adds to front
    put (DL.snoc xs result)



main :: IO()
main = mapM_ putStrLn $ fizzBuzzList [1 .. 100]
