import Data.Monoid (Monoid, (<>), Sum)
import Control.Applicative
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative)



newtype Sum a = Sum { getSum :: a } deriving (Eq, Show)
data Three a b c = Three a b c deriving (Eq, Show)


instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure x = Three mempty mempty x
    (<*>) (Three a b f) (Three a' b' c) = Three (a <> a') (b <> b') (f c)



instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq



t1 = pure 1 :: Three String String Int
t2 = (Three "" "" (*9)) <*> (Three "" "" 3)
-- t2 = (Three (Sum 1) (Sum 2) (*9)) <*> (Three 1 2 3)


main = do
    print t1
    print t2
    --quickBatch $ applicative (undefined :: Three String String Int)
    --quickBatch $ applicative (Three ("b", "w", [1]) ("b", ["w"], [1]) (["b"], "w", 1))
    quickBatch $ applicative (undefined :: Three String (String, String, [Int]) (Int, Double, Char))
    -- HELP what does this type mean and why does it work?