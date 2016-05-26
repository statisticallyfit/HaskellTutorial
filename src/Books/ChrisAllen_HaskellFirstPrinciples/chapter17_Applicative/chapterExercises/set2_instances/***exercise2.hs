import Data.Monoid (Monoid, (<>))
import Control.Applicative
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative)



data Pair a = Pair a a deriving (Eq, Show)


instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')


instance Applicative Pair where
    pure a = Pair a a
    (<*>) (Pair f1 f2) (Pair x1 x2) = Pair (f1 x1) (f2 x2)



instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        return (Pair a a)

instance Eq a => EqProp (Pair a) where
    (=-=) = eq


t1 = (Pair (+1) (*3)) <*> (Pair 4 5)


main = do
    print t1
    quickBatch $ applicative (undefined :: Pair (String,String,Int))
    -- note also works: (Int, Char, Double)
    -- HELP why aren't the (string,string,int) part repeated twice for Pair a a?
    -- now it looks like it's just Pair a instead of Pair a a
    -- HELp also why are there three types - string,string, and int?
    -- why not just have for the first a = Int and for second a = String?