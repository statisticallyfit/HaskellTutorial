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
data Operation = Number    -- 1,2,3,4,5,6,7,8,9,0
               | Character -- a,b,c,...z and A,B,C,...Z (only if capitalized)
               | Spacebar  -- ' '
               | Capitalize -- the next one after capitalize will be default lowercase.
               deriving (Eq, Show)
data Button = Button Operation Symbol deriving (Eq, Show)
data Phone = PhonePad [Button] deriving (Eq, Show)




text :: [String]
text =
    ["The night sky is littered with stars.",
     "Sparkling water.",
     "Swallows descend from their nightly perches and race in the grey dawn.",
     "Mountains tower, their craggy faces rough and weather-hewn.",
     "Twittering birds.",
     "Soaring eagles.",
     "Morning dew settles on lilac bushes, and fog rises off the lake in golden splendor.",
     "Volcanic ash.",
     "Wind scours, molds, and shapes, leaving traced frozen lava in its wake.",
     "Glowing crystals."]



displayPhone :: IO()
displayPhone = putStr $ break ++ row123 ++ break ++ row456 ++ break ++
                   row789 ++ break ++ rowOp ++ break
    where break  = "-------------------------------------\n"
          row123 = "|   1       |   2 ABC   |   3 DEF   |\n"
          row456 = "|   4 GHI   |   5 JKL   |   6 MNO   |\n"
          row789 = "|   7 PQRS  |   8 TUV   |   9 WXYZ  |\n"
          rowOp  = "|   * ^     |   0 + _   |   # .,    |\n"



{-
-- what does this do????
-- note symbol can be 'a', 'A', '9', '*', '0'
reverseTaps :: Phone -> Symbol -> [(Digit, Presses)]
reverseTaps p sym
    | isLetter sym =
    | isDigit sym =
-}


phone = Phone
    [('1', "1"),
     ('2', "abc2"),
     ('3', "def3"),
     ('4', "ghi4"),
     ('5', "jkl5"),
     ('6', "mno6"),
     ('7', "pqrs7"),
     ('8', "tuv8"),
     ('9', "wxyz9"),
     ('*', "^"),
     ('0', "_0"),
     ('#', ".,")
    ]


{-
Inspiration links:

https://github.com/Tclv/HaskellBook/blob/master/ch11/Phone.hs
https://github.com/dwayne/haskell-programming/blob/master/ch11/Phone.hs
https://github.com/vaughanj10/haskell_programming/blob/master/ch11/phone.hs
https://github.com/juank-pa/haskell-training/blob/master/Chapter11/Exercises/DaPhone.hs

-}