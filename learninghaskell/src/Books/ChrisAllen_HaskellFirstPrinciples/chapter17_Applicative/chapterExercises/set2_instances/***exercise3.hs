import Data.Monoid (Monoid, (<>))
import Control.Applicative
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative)


data Two a b = Two a b deriving (Eq, Show)


instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure x = Two mempty x
    (<*>) (Two a f) (Two a' b) = Two (a <> a') (f b)


-- note: Eq has kind (*) so use (Two a b) to reduce kind of type Two
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

-- note: Eq has kind (*) so use (Two a b) to reduce kind of type Two
instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq



t1 = pure 2 :: Two String Int
t2 = (Two "horse" (+3)) <*> (Two " racer" 2)


main = do
    print t1
    print t2
    -- HELP HELP HELP how to give test args to get this working?
    --quickBatch $ applicative (undefined :: Two (String,Int)(String,Int))
    --quickBatch $ applicative (undefined :: Two (String,String,Int))
    --quickBatch $ applicative (Two ("b","w",[1]) ("b",["w"],1))
    quickBatch $ applicative (undefined :: Two String (Int, Double,Char))
    -- HElp why does this work but others don't?
    -- quickBatch $ applicative (undefined :: Two (String,String,Int)(String,String,Int))

