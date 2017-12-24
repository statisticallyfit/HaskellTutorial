import Prelude hiding (drop, splitAt)
import qualified Prelude
import Test.QuickCheck




drop :: Int -> [a] -> [a]
drop 0 xs     = xs
drop _ []     = []
drop n (x:xs) = drop (n-1) xs


propDrop :: (Positive Int) -> [Int] -> Bool
propDrop (Positive n) xs = drop n xs == Prelude.drop n xs



{-

splitAt          :: Int -> [a] -> ([a], [a])
splitAt 0 xs     = ([], xs)
splitAt _ []     = ([], [])
splitAt n (x:xs) = x : splitAt (n-1) xs-}


splitAt      :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

propSplitAt :: (Positive Int) -> [Int] -> ([Int], [Int])
propSplitAt (Positive n) xs = splitAt n xs == Prelude.splitAt n xs


main = do
    quickCheck propDrop
    --quickCheck propSplitAt -- HELP weird, works in GHC