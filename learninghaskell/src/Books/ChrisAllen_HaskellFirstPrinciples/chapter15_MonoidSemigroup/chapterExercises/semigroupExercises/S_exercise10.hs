--import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>), Sum(..))

-- Given a data type implement the Monoid instance.
-- Add Monoid constraints to type variables where needed


-- help: how do the args Four a b c d get passed in here: which is in what place?
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


-- help: meaning of the curl braces.n
newtype Comp a = Comp {unComp :: (a -> a) } --deriving (Eq, Show)



f = Comp $ \(Sum n) -> Sum (n + 1)
g = Comp $ \(Sum n) -> Sum (n - 1)


-- help meaning of this type definition?
instance Semigroup a => Semigroup (Comp a) where
    Comp {unComp = f} <> Comp {unComp = g} = Comp (f <> g) -- help meaning?




main = do
    print $ unComp (f <> g) $ 0
    print $ unComp (f <> g) $ 1
    print $ unComp (f <> f) $ 1
    print $ unComp (g <> f) $ 1
