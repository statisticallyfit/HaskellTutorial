module Phone where

import Data.Char
import Data.List
import Data.Maybe


{-
IMPORTANT RULES:
1.(*) is capitalization of letter
2. (0) is space bar
3. to get a digit, push the buton as many times as digit's location. Do wraparound so
more presses than digits in button get you back around to letters on button.
-}


type Token = Char
type Presses = Int -- >= 1
type FingerMoves = [(Token, Presses)]

-- note distinguishing between uppercase and lower case so there is no more
-- need to check in functions if the arg Token in Button is lower or uppercase.
-- note SwitchFormat refers to switching from letters + punctuation to numbers
-- and vice versa. If before was nums, then say Switch, then switch to english.
-- example: to make uppercase letter, put Upper in list before a button letter.
data Button = SwitchFormat | Upper | Lower | Space
            | Button Number Token
            | Button Letter Token
            | Button Sign Token
            deriving (Eq, Show)

data Phone = PhonePad [Button] deriving (Eq, Show)



phone :: Phone
phone = PhonePad $ concat [numbers, lowLetters, uppLetters, signs]
    where numbers = map (Button $ Number) (concatMap show [0..9])
          lowLetters = map (Button $ LowerLetter) ['a' .. 'z']
          uppLetters = map (Button $ UpperLetter) ['A' .. 'Z']
          signs = map (Button $ Sign) "*^+ #,.?!" -- note has blank space for space.

keyPad :: [String]
keyPad = ["abc", "def", "ghi", "jkl", "mno", "pqrs", "tuv", "wxyz",
          "*^", "+0 ", "#.,?!"]

text :: [String]
text =
    ["The night sky is littered with stars.",
     "Swallows sing in the grey dawn.",
     "Morning dew settles on lilac bushes",
     "Fog on the lake rises in golden splendor.",
     "Traced frozen lava scoured by winds.",
     "Glowing crystal caves."
     "*^+ #,.?!123abc123..?.abc"] -- tests switching

displayPhone :: IO()
displayPhone = putStr $ break ++ row123 ++ break ++ row456 ++ break ++
                   row789 ++ break ++ rowOp ++ break
    where break  = "-------------------------------------\n"
          row123 = "|   1       |   2 ABC   |   3 DEF   |\n"
          row456 = "|   4 GHI   |   5 JKL   |   6 MNO   |\n"
          row789 = "|   7 PQRS  |   8 TUV   |   9 WXYZ  |\n"
          rowOp  = "|   * ^     |   0 + _   |   # .,?!  |\n"

-- only expects the Token present in the phonepad: letters,nums, *+-^#,.'?!
toButton :: Token -> Button
toButton n
    | isLetter n && isLower n = Button LowerLetter n
    | isLetter n && isUpper n = Button UpperLetter n
    | isDigit n = Button Number n
    | elem n "*^+ #,.?!" = Button Sign n
    | otherwise = Button Unknown ' '

-- note Token can be 'a', 'A', '9', '*', '0'
-- note called reverseTaps previously
-- note press (*) to capitalize, and (+) to switch back and forth from nums to letters.
-- postcondition: returns exception head if given char it doesn't know.
{-
toTaps :: Button -> [(Token, Presses)]
toTaps (Button Number n) = (n, 1)
toTaps (Button Letter n) = (n, getTaps n)
toTaps (Button Sign n) = classifySign n
    where getTaps n = 1 + head $ catMaybes (map (elemIndex n) keyPad)
          classifyNum n =
-}





{-
Inspiration links:

https://github.com/Tclv/HaskellBook/blob/master/ch11/Phone.hs
https://github.com/dwayne/haskell-programming/blob/master/ch11/Phone.hs
https://github.com/vaughanj10/haskell_programming/blob/master/ch11/phone.hs
https://github.com/juank-pa/haskell-training/blob/master/Chapter11/Exercises/DaPhone.hs

-}