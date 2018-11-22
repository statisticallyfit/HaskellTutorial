import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid, Sum)
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


-- help must declare Sum here because haskell can't find it....
newtype Sum a = Sum { getSum :: a } deriving Show
newtype Comp a = Comp {unComp :: (a -> a)}-- deriving (Eq, Show)

-- help meaning of this type definition?
instance Semigroup a => Semigroup (Comp a) where
    Comp {unComp=f} <> Comp {unComp=g} = Comp (f <> g)
    --Comp (f) <> Comp (g) = Comp (f <> g) -- help meaning?


-- HELP why both semigroup a and monoid a why not just monoid a?
instance (Semigroup a, Monoid a) => Monoid (Comp a) where
    mempty = Comp id -- help why isn't Combine defined with ID as well?
    mappend = (<>)




f = Comp $ \(Sum n) -> Sum (n + 1)
g = Comp $ \(Sum n) -> Sum (n - 1)



main = do
  print $ unComp (f <> g ) $ 0
  print $ unComp (f <> g ) $ 1
  print $ unComp (f <> f ) $ 1
  print $ unComp (g <> f ) $ 1
  print $ unComp (mappend mempty f) 0 == unComp f 0