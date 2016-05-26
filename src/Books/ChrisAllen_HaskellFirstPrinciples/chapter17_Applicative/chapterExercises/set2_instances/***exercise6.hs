import Data.Monoid (Monoid, (<>), Sum)
import Control.Applicative
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative)



data Four a b c d = Four a b c d deriving (Eq, Show)


instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure x = Four mempty mempty mempty x
    (<*>) (Four a b c f) (Four a' b' c' d) = Four (a <> a')
                                                  (b <> b')
                                                  (c <> c')
                                                  (f d)


instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return (Four a b c d)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq



t1 = pure 1 :: Four String String String Int
t2 = (Four "the" "surfed" "the deep" (*8)) <*> (Four " whale" " over" " blue waves" 9)


main = do
    print t1
    print t2
    -- HELP can't figure out why not working. All others have the same pattern.
  -- quickBatch $ applicative (undefined :: Four String (String, String, [Int]) (Int, [Int], String) ([Int],String,Int))