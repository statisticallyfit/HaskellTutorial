import Test.QuickCheck (Arbitrary, arbitrary, quickCheck)
import Data.Semigroup (Semigroup, (<>))

-- Given a data type implement the Monoid instance.
-- Add Monoid constraints to type variables where needed

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


data Two a b = Two a b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two c d) = Two (a <> c) (b <> d)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)


type TwoAssoc = Two String Ordering -> Two String Ordering
                -> Two String Ordering -> Bool
-- help: why choose string and ordering? Why does int and double not work?



{-
note: this is how I tested what arbitrary means:

*Main> :{
*Main| data Two a b = Two a b
*Main|     deriving (Eq, Show)
*Main| :}
*Main> :{
*Main| instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
*Main|     arbitrary = do
*Main|         a <- arbitrary
*Main|         b <- arbitrary
*Main|         return (Two a b)
*Main| :}
*Main> import Test.QuickCheck

sample (arbitrary :: Gen (Two String Ordering))
Two "" EQ
Two "_" LT
Two "1K\188" EQ
Two "K}\162\SUB\NUL\FS" EQ
Two "k" GT
Two "5fh=]^\211=r" LT
Two ">w" EQ
Two "[2" LT
Two "\170\251?|\204w\145" GT
Two "\DC2-\175\149Q\173" EQ
Two "%G\146)~\DEL\DC4!t\SI5\148Sm\US\216=.@" LT

-}



main :: IO()
main = do
    quickCheck (semigroupAssoc :: TwoAssoc)