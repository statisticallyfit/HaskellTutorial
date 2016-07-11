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


type Symbol = Char
--type Digit = Char
type Presses = Int -- >= 1

-- note distinguishing between uppercase and lower case so there is no more
-- need to check in functions if the arg Symbol in Button is lower or uppercase.
data Operation = Number    -- 1,2,3,4,5,6,7,8,9,0
               | LowerLetter -- a,b,c,...z and
               | UpperLetter -- A,B,C,...Z (only if capitalized)
               | Sign -- ?!.,
               | Unknown -- not in phone : & % @ $
               deriving (Eq, Show)
data Button = SwitchFormat -- to change from letters + punct to nums and vice versa.
            | Button Operation Symbol deriving (Eq, Show)
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
     "Glowing crystal caves."]

displayPhone :: IO()
displayPhone = putStr $ break ++ row123 ++ break ++ row456 ++ break ++
                   row789 ++ break ++ rowOp ++ break
    where break  = "-------------------------------------\n"
          row123 = "|   1       |   2 ABC   |   3 DEF   |\n"
          row456 = "|   4 GHI   |   5 JKL   |   6 MNO   |\n"
          row789 = "|   7 PQRS  |   8 TUV   |   9 WXYZ  |\n"
          rowOp  = "|   * ^     |   + 0 _   |   # .,?!  |\n"

-- only expects the symbols present in the phonepad: letters,nums, *+-^#,.'?!
toButton :: Symbol -> Button
toButton n
    | isLetter n && isLower n = Button LowerLetter n
    | isLetter n && isUpper n = Button UpperLetter n
    | isDigit n = Button Number n
    | elem n "*^+ #,.?!" = Button Sign n
    | otherwise = Button Unknown ' '

-- note symbol can be 'a', 'A', '9', '*', '0'
-- note called reverseTaps previously
-- note press (*) to capitalize, and (+) to switch back and forth from nums to letters.
-- postcondition: returns exception head if given char it doesn't know.
{-
toTaps :: Button -> [(Symbol, Presses)]
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