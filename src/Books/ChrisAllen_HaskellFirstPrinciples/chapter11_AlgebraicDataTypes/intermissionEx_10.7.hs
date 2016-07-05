

--- 1
data PugType = PugData
-- cardinality = 1

--- 2
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
-- cardinality = 3


--- 3
-- cardinality == 65535 of type Int16
{-
*Main> import Data.Int
*Main Data.Int> minBound :: Int16
-32768
*Main Data.Int> maxBound :: Int16
32767
*Main Data.Int> 32768 + 32767
65535
-}









--- 1
data Example = MakeExample deriving Show

 {-
*Main> :t MakeExample
MakeExample :: Example
*Main> :t E
EQ       Either   Enum     Eq       Example
*Main> :t Example

<interactive>:1:1: Not in scope: data constructor `Example'

 -}



--- 2 instance of Show typeclass

--- 3
data Instance = MakeInstance Int deriving Show

{-
*Main> :t MakeInstance
MakeInstance :: Int -> Instance
*Main> :t Instance

<interactive>:1:1: Not in scope: data constructor `Instance'
*Main> :k Instance
Instance :: *

-}