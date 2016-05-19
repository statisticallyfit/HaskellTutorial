import Test.QuickCheck (Arbitrary, arbitrary, quickCheck)
import Data.Semigroup (Semigroup, (<>))

-- Given a data type implement the Monoid instance.
-- Add Monoid constraints to type variables where needed

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)



newtype Identity a = Identity a
    deriving (Show, Eq)


instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)


instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)

{- help: result if I do:
help what does this mean?
help: what does a <- arbitrary mean?
*Main Test.QuickCheck> sample (arbitrary :: Gen (Identity String))
Identity ""
Identity ""
Identity "\225Gi1"
Identity "'\225sn\233"
Identity "\134n#1\248@\r"
Identity "\209\169\fI"
Identity "\186qU}1\191\221;T\226\SO>"
Identity "k\190\184\GS\140\254"
Identity "~-\STXJ\RSk\237\&3\NAKFWR"
Identity "$D\EOT\137S\170\DEL\SOH\235\ENQ\USz\GS`\146\248"
Identity "/R\n7\ETB\RS\240\169\DC3"
-}

{-
note: steps to test what arbitrary does:
1. go to the file (cd /..)
2. write : newtype Identity a ... (to declare it)
3. let arbitrary = ...
4. import Test.QuickCheck
5. sample (arbitrary :: Gen Identity)
-}

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

main :: IO()
main = do
    quickCheck (semigroupAssoc :: IdentityAssoc)
