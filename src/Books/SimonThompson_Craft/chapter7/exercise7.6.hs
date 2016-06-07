import Prelude hiding (and, or)
import qualified Prelude
import Test.QuickCheck

and, or    :: [Bool] -> Bool

and []     = True
and (b:bs) = b && and bs

or []      = False
or (b:bs)  = b || or bs


propAnd bs = and bs == Prelude.and bs
propOr bs  = or bs  == Prelude.or bs


main = do
    quickCheck propAnd
    quickCheck propOr