module Phone where

import Data.Char

{-
IMPORTANT RULES:
1.(*) is capitalization of letter
2. (0) is space bar
3. to get a digit, push the buton as many times as digit's location. Do wraparound so
more presses than digits in button get you back around to letters on button.
-}

type Digit = Char
type Symbol = Char
type Presses = Int -- >= 1
data Button = Button Digit Symbol deriving (Eq, Show)
data Phone = PhonePad [Button] deriving (Eq, Show)








text :: [String]
text =
    ["The night sky is littered with stars.",
     "Sparkling water",
     "Swallows descend from their nightly perches and race in the grey dawn",
     "Mountains tower, their craggy faces rough and weather-hewn",
     "Twittering birds",
     "Soaring eagles",
     "Morning dew settles on lilac bushes, and fog rises off the lake in golden splendor",
     "Volcanic ash",
     "Wind scours, molds, and shapes, leaving traced frozen lava in its wake.",
     "Glowing crystals"]



phone :: IO()
phone = putStr $ break ++ row123 ++ break ++ row456 ++ break ++
                   row789 ++ break ++ rowOp ++ break
    where break  = "-------------------------------------\n"
          row123 = "|   1       |   2 ABC   |   3 DEF   |\n"
          row456 = "|   4 GHI   |   5 JKL   |   6 MNO   |\n"
          row789 = "|   7 PQRS  |   8 TUV   |   9 WXYZ  |\n"
          rowOp  = "|    * ^    |   0 + _   |    # .,   |\n"