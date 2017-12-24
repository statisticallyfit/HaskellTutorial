import Data.Monoid (Monoid, (<>), Sum)
import Control.Applicative
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative)



-- NOTE HELP so :k measures the kind of the data not the constructor since
-- :k Four' ==> * -> * -> * ==> a -> b -> RESULT
data Four' a b = Four' a a a b deriving (Eq, Show)


instance Functor (Four' a) where
    fmap f (Four' a0 a1 a2 b) = Four' a0 a1 a2 (f b)

instance Monoid a => Applicative (Four' a) where
    pure x = Four' mempty mempty mempty x
    (<*>) (Four' a0 a1 a2 f) (Four' a0' a1' a2' b) = Four' (a0 <> a0')
                                                           (a1 <> a1')
                                                           (a2 <> a2')
                                                           (f b)



instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Four' a a a b)

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq




t1 = pure 1 :: Four' String Int
t2 = (Four' "hi " "how " "you " (+4)) <*> (Four' "there" "are" "today" 17)
-- HELP why error when (-4) is used instead? Doesn't it understand (-)?


main = do
    print t1
    print t2
    quickBatch $ applicative (undefined :: Four' String  (Int,Char,Double))
    -- HELP following the data pattern (2 args) not the constructor's (4 args)

