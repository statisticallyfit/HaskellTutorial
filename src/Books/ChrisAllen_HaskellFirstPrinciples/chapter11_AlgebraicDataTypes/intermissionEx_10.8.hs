import Data.Int


--- 1
data BigSmall = Big Bool | Small Bool
-- cardinality == 4


--- 2
data NumberOrBool = Numba Int8 | BoolyBool Bool
--- cardinality == 3 + (127 + 128) = 258