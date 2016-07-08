
import Test.QuickCheck


{-
note types

*Main> :t choose
choose :: System.Random.Random a => (a, a) -> Gen a
*Main> :t elements
elements :: [a] -> Gen a
*Main> :t sample'
sample' :: Gen a -> IO [a]
*Main> :t sample
sample :: Show a => Gen a -> IO ()

-}


oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a,b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a,b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a,b,c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a,b,c)


genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a] -- note equal probability


genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [(1, return Nothing),
               (3, return (Just a))] -- note in ratio of 1 to 3 so 1/4 Nothings.


intsSample = sample oneThroughThree
boolSample = sample genBool
bool'Sample = sample genBool'
ordSample = sample genOrdering
charSample = sample genChar
tupSample = sample (genTuple :: Gen (Int, Float))
threepleSample = sample (genThreeple :: Gen (Char, Int, Float))
eitherSample = sample (genEither :: Gen (Either Int Float))
maybeSample = sample (genMaybe :: Gen (Maybe Char))
maybeSample' = sample (genMaybe' :: Gen (Maybe Float))
