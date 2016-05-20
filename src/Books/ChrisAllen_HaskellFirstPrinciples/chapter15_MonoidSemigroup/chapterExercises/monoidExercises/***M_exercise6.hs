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
newtype Combine a b = Combine { unCombine :: (a -> b)} deriving (Eq, Show)


instance Semigroup b => Semigroup (Combine a b) where
    Combine {unCombine = f} <> Combine {unCombine = g} = Combine (f <> g)


instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty = Combine mempty
    mappend = (<>)


f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)


type CombineAssoc = Combine String Ordering -> Combine String Ordering ->
                    Combine String Ordering -> Bool




main = quickCheck (semigroupAssoc :: CombineAssoc)
--main = do
  --  print $ Combine {unCombine=f} <> Combine {unCombine=g}
    --print $ unCombine (f <> g) $ 0
