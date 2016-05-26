import Control.Applicative (liftA3)
import Control.Monad (return)
{-
Given the following sets of consonants and vowels:

stops = "pbtdkg"
vowels = "aeiou"

a) Write a function that takes inputs from stops and vowels
and makes 3-tuples of all possible stop-vowel-stop combinations.
These will not all correspond to real words in English,
although the stop-vowel-stop pattern is common enough
that many of them will.

b) Modify that function so that it only returns the combinations
that begin with a p.

c) Now set up lists of nouns and verbs (instead of stops and
vowels) and modify the function to make tuples representing
possible noun-verb-noun sentences.
-}

{-
instance (Show a) => Show (IO a) where
    show (IO a) = "" ++ a-}


stops, vowels :: String
stops = "pbtdkg"
vowels = "aeiou"

combos' :: [String]
combos' = [[a] ++ [b] ++ [c] | a <- stops, b <- vowels, c <- stops]
-- note works with a:b:c[] too instead

--combos :: [a] -> [b] -> [c] -> [(a,b,c)] -- note tuple of 3 chars
combos = (,) <$> firstTwoLetters <*> stops
    where firstTwoLetters = ((,) <$> stops <*> vowels)

-- HELP understand liftA3 better:
-- todo https://wiki.haskell.org/Applicative_functor


{- NOTE
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c

-}



