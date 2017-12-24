import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- avoid orphan instances


instance Monoid a => Monoid (ZipList a) where
    mempty = ZipList []
    mappend = liftA2 mappend -- HELp meaning?

-- HElp why use arbitrary here but not with the CheckersQuickcheck file?
-- HELP why declare it this way if Arbitrary has kind (*) but ZipList has kind * -> * ??
instance Arbitrary a => Arbitrary (ZipList a) where
    arbitrary = ZipList <$> arbitrary


-- HELP why declare it this way if Arbitrary has kind (*) while Sum has kind * -> * ??
instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = Sum <$> arbitrary


instance Eq a => EqProp (ZipList a) where (=-=) = eq


main = quickBatch (monoid (ZipList [1 :: Sum Int]))
