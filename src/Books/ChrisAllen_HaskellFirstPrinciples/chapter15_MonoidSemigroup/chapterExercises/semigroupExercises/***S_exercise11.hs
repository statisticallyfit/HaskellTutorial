import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))




-- help: how do the args Four a b c d get passed in here: which is in what place?
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)



{-
help: why is 'a' used everywhere, like why in 3rd statement can't 'b' be used
instead of 'a'?
    answer: because Failure is declared to have type a in the data definition.
help: what does last line mean?
help: why state "Semigroup a" only and not also for b?

help: explain meaning of:

:t (Failure "hi") <> (Failure "hi there")
(Failure "hi") <> (Failure "hi there") :: Validation [Char] b

and

:t (Success "hi") <> (Success "hi there")
(Success "hi") <> (Success "hi there") :: Semigroup a => Validation a [Char]
-}

data Validation a b = Failure a | Success b
    deriving (Eq, Show)


instance Semigroup a => Semigroup (Validation a b) where
    (Failure a) <> (Failure b) = Failure (a <> b)
    (Failure a) <> _           = Failure a
    _           <> (Failure a) = Failure a
    a           <> _           = a

{-
instance (Show a, Show b) => Show (Validation a b) where
    show (Failure a) = "Failure " ++ show a
    show (Success b) = "Success " ++ show b
-}

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [(Success a), (Failure b)] -- help meaning


type ValidationAssoc = Validation String Ordering ->
     Validation String Ordering -> Validation String Ordering -> Bool



main :: IO()
main = do
    quickCheck (semigroupAssoc :: ValidationAssoc)
    putStrLn ""
    print $ (Failure "hi" :: [Char]) <> (Failure "abc" :: [Char])
    -- help why can't this print? why error?