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
-- note Lower in front means lower all the rest behind it. Same with Upper.
-- Same with NumPad and EngPad. Default remains, is not switched unless stated.
-- example: To Swtich from capital to lower press * after a capital and vice versa.
-- example: to make uppercase letter, put Upper in list before a button letter.
data Button = NumPad | EngPad | Uppercase | Lowercase | Spacebar
            | Number Token
            | Letter Token
            | Sign Token
            | Unknown -- for symbols outside space + . , ? ! #
            deriving (Eq, Show)

data Phone = PhonePad [Button] deriving (Eq, Show)



{-
  * --> capitalize -> ('*',1)
  * -> lowercase -> ('*', 1) -- note switchcase to opposite than one before it.
  ^ -> switch format -> ('*',2)
  0 -> spacebar -> ('0',3)
  + -> plus sign -> ('0', 2)
  # -> questionmark -> ('#', 4)
  The only symbols that exist are: space + . , ? ! #
-}
phone :: Phone
phone = PhonePad $ concat [numbers, lowLetters, uppLetters, signs, space]
    where numbers = map (Number $) (concatMap show [0..9])
          lowLetters = [Lowercase] ++ map (Letter $) ['a' .. 'z']
          uppLetters = [Uppercase] ++ map (Letter $) ['A' .. 'Z']
          signs = map (Sign $) "+.,?!#"
          space = [Spacebar]




text :: [String]
text =
    ["The night sky is littered with stars.",
     "Swallows sing in the grey dawn.",
     "Morning dew settles on lilac bushes",
     "Fog on the lake rises in golden splendor.",
     "Traced frozen lava scoured by winds.",
     "Glowing crystal caves.",
     "*^+ #,.?!123abc123..?.abc"] -- tests switching

phonePad :: IO()
phonePad = putStr $ break ++ row123 ++ break ++ row456 ++ break ++
                   row789 ++ break ++ rowOp ++ break
    where break  = "-------------------------------------\n"
          row123 = "|   1       |   2 ABC   |   3 DEF   |\n"
          row456 = "|   4 GHI   |   5 JKL   |   6 MNO   |\n"
          row789 = "|   7 PQRS  |   8 TUV   |   9 WXYZ  |\n"
          rowOp  = "|   * ^     |   0 + _   |   # .,?!  |\n"


{-
Number format:
presses = 1 for each: 0..9
no symbols at bottom of phone pad exist for numbers.

Letter format:
presses example 'e' = ('3', 2) which means press 3 twice.
* --> capitalize -> ('*',1)
^ -> switch format -> ('*',2)
0 -> spacebar -> ('0',3)
+ -> plus sign -> ('0', 2)
# -> questionmark -> ('#', 4)
The only symbols that exist are: space + . , ? ! #
-}


getToken :: [Button] -> Token
getToken [EngPad, Lowercase, Letter n] = n
getToken [EngPad, Uppercase, Letter n] = toUpper n
getToken [NumPad, Number n] = n
getToken [EngPad, Sign n] = n
getToken [Spacebar] = ' '

-- EngPad = 2 presses to get to '^' char
-- Lowercase or Uppercase = 1 press to press '*' char
-- letter == depends on its location
-- spacebar == press 0 three times.
getPresses :: [Button] -> Presses
getPresses [EngPad, _, Letter n] = 3 + countKey n
getPresses [EngPad, _, Sign n] = 3 + countKey n
getPresses [NumPad, _] = 2
getPresses [Spacebar] = 3 -- press 0 three times.
    where countKey n = 
          keyPad = ["abc","def","ghi","jkl","mno","pqrs","tuv","wxyz","+.,?!#"]


-- note returns just one set of puttons - not for whole sentence
tokenToButton :: Token -> [Button]
tokenToButton tok
    | isUpper tok = EngPad : Uppercase : Letter (toLower tok) : []
    | isLower tok = EngPad : Lowercase : Letter tok : []
    | isDigit tok = NumPad : Number tok : []
    | isSpace tok = Spacebar : []
    | isSign tok  = EngPad : Sign tok : []
    | otherwise = [Unknown]
    where isSign t = elem t "+.,?!#"

-- note
{-fingerToButton :: FingerMoves -> [Button]
fingerToButton-}
-- only expects the Token present in the phonepad: letters,nums, *+-^#,.'?!
-- note expect list length no greater than 2.


{-toButton :: FingerMoves -> Button
toButton (token, presses)

toButton [(token, presses)]
    | isNumber token = Number token -- check presses == 1
    | isLetter


    | isLetter n && isLower n = Button LowerLetter n
    | isLetter n && isUpper n = Button UpperLetter n
    | isDigit n = Number n
    | elem n "*^+ #,.?!" = Button Sign n
    | otherwise = Button Unknown ' '

-- note Token can be 'a', 'A', '9', '*', '0'
-- note called reverseTaps previously
-- note press (*) to capitalize, and (+) to switch back and forth from nums to letters.
-- postcondition: returns exception head if given char it doesn't know.

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