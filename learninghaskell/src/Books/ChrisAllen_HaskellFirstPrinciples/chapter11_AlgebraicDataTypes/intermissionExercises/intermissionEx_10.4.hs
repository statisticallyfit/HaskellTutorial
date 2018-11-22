

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

data DogueDeBordeaux doge = DogueDeBordeaux doge
{-
*Main> :k Doggies
Doggies :: * -> *
*Main> :k Doggies String
Doggies String :: *
*Main> :t Husky 10
Husky 10 :: Num a => Doggies a
*Main> :t Husky
Husky :: a -> Doggies a
*Main> :t Husky (10::Integer)
Husky (10::Integer) :: Doggies Integer
*Main> :t Mastiff "Scooby Doo"
Mastiff "Scooby Doo" :: Doggies [Char]

*Main> :t DogueDeBordeaux
DogueDeBordeaux :: doge -> DogueDeBordeaux doge
*Main> :t DogueDeBordeaux "doggie!"
DogueDeBordeaux "doggie!" :: DogueDeBordeaux [Char]
*Main> :k DogueDeBordeaux
DogueDeBordeaux :: * -> *

-}