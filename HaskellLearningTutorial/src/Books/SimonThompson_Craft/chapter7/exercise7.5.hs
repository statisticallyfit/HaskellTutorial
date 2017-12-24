import Prelude hiding (product)
import qualified Prelude
import Test.QuickCheck

product        :: [Int] -> Int
product []     = 1
product (x:xs) = x * product xs


propProduct xs = product xs == Prelude.product xs

main = do
    quickCheck propProduct