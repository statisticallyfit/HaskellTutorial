
import Control.Applicative
import Data.Monoid (Monoid, (<>))
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Modifiers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.All (applicative, monad)
-- implement the either monad


-- note: todo: come back to do this once I cover previous chapters too:
-- https://lukleh.github.io/haskell-book-exercises/#_18_4_examples_of_monad_use


data Sum a b = First a | Second b
    deriving (Eq, Show)


instance Functor (Sum a) where
    fmap = undefined

instance Applicative (Sum a) where
    pure = undefined
    (<*>) = undefined

instance Monad (Sum a) where
    return = pure
    (>>=) = undefined


main = do
    print $ ""