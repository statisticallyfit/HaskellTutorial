import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>))

-- Given a data type implement the Monoid instance.
-- Add Monoid constraints to type variables where needed


-- help: how do the args Four a b c d get passed in here: which is in what place?
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)



data Or a b = Fst a | Snd b
    deriving (Eq, Show)


instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    (Snd a) <> _ = Snd a
    _ <> (Snd a) = Snd a
    _ <> b       = b -- help what does this mean, result in?


instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [(Fst a), (Snd b)] -- help meaning?


type OrAssoc = Or String Ordering -> Or String Ordering -> Or String Ordering -> Bool
-- help why these not other types?



{-
NOTE HELP - meaning?

*Main Test.QuickCheck> :{
*Main Test.QuickCheck| data Or a b = Fst a | Snd b
*Main Test.QuickCheck|     deriving (Eq, Show)
*Main Test.QuickCheck|
*Main Test.QuickCheck| instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
*Main Test.QuickCheck|     arbitrary = do
*Main Test.QuickCheck|         a <- arbitrary
*Main Test.QuickCheck|         b <- arbitrary
*Main Test.QuickCheck|         elements [(Fst a), (Snd b)]
*Main Test.QuickCheck| :}
*Main Test.QuickCheck> sample (arbitrary :: Gen (Or String Ordering))
Fst ""
Snd GT
Fst "vp\243"
Fst "H\DLE\149"
Snd EQ
Fst "'\134\233\194FSCO!H"
Fst "D\n\247\160ND/"
Snd EQ
Snd LT
Fst "Ad\v,\NAK\EM(9f&e}\v"
Fst "$\168/\ACK\246\SYN\130\150"
*Main Test.QuickCheck>

-}


main :: IO()
main =
    quickCheck (semigroupAssoc :: OrAssoc)