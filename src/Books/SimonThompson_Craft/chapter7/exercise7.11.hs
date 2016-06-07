import Test.QuickCheck
import Prelude hiding (reverse, unzip)
import qualified Prelude


reverse        :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

propReverse xs = reverse xs == Prelude.reverse xs



unzip :: [(a,b)] -> ([a], [b])
unzip xys = ([x | (x,_) <- xys], [y | (_,y) <- xys])


unzip'             :: [(a,b)] -> ([a], [b])
unzip' []          = ([], [])
unzip' ((a,b):abs) = ([a] ++ fst unzippedRest, [b] ++ snd unzippedRest)
                  where unzippedRest = unzip' abs


propUnzip xs = unzip xs == Prelude.unzip xs


{-
uncover - help why not working - test in ghc instead, it works.
main = do
    quickCheck propReverse
    quickCheck propUnzip
-}
