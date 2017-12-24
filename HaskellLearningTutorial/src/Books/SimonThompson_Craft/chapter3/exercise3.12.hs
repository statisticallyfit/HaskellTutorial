import Data.Char
import Data.Angle (sine, cosine, tangent, degrees, radians)

offset :: Int
offset = ord 'A' - ord 'a'

toCapital :: Char -> Char
toCapital c
    | isLetter(c) = chr (ord c + offset) -- turn to int, offset, turn to chr
    | otherwise   = c


charToNum :: Char -> Int
charToNum c
    | isDigit(c) = ord c - (ord '0')
    | otherwise  = -999


main = do
    print (toCapital 'n')
    print (toCapital '9')
    print (charToNum '5')
    print (charToNum 'm')