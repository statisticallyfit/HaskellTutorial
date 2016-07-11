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
type FingerMove = (Token, Presses)
type FingerMoveGroup = [FingerMove] -- a group of fingermoves for 1 letter/num/sign

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
type ButtonGroup = [Button] -- note a group of buttons for 1 letter/num/sign
data Phone = PhonePad [Button] deriving (Eq, Show)



{-
  * --> capitalize -> ('*',1)
  * -> lowercase -> ('*', 1) -- note switchcase to opposite than one before it.
  ^ -> switch format -> ('*',2)
  0 -> spacebar -> ('0',2)
  + -> plus sign -> ('0', 1) -- once it is understood we are in LetterFOrmat
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
Number format: ---------------------------------------------------------------------
presses = 1 for each: 0..9
no symbols at bottom of phone pad exist for numbers.


Letter format: ---------------------------------------------------------------------
RULE key: whenever there is a number in front during letter format, we ignore it
and start counting presses from second non-number element (see '+' and ' ' below)

presses example 'e' = ('3', 2) which means press 3 twice.
* --> capitalize -> ('*',1)
^ -> switch format -> ('*',2)
0 -> spacebar -> ('0',2)
+ -> plus sign -> ('0', 1)
? -> questionmark -> ('#', 4)
The only symbols that exist are: space + . , ? ! #
-}


getToken :: ButtonGroup -> Token
getToken [Lowercase, Letter n] = n
getToken [Uppercase, Letter n] = toUpper n
getToken [Number n] = n
getToken [Sign n] = n
getToken [Spacebar] = ' '


-- Lowercase or Uppercase = 1 press to press '*' char
-- letter == depends on its location
-- spacebar == press 0 two times.
getPresses :: ButtonGroup -> Presses
getPresses [_, Letter n] = 2 + countKey n
getPresses [_, Sign n] = 2 + countKey n
getPresses [Number _] = 1
getPresses [Spacebar] = 2 -- press 0 two times. (knowing it is LETTER FORMAT)

countKey :: Token -> Presses
countKey tok = 1 + head (catMaybes (map (elemIndex tok) (map snd keyPad)))

tokenWithPresses :: Token -> FingerMove
tokenWithPresses tok = (fst $ head $ filter ((elem tok) . snd) keyPad, countKey tok)

tokenFromPresses :: FingerMove -> Token
tokenFromPresses (tok, presses) = alphs !! (presses - 1)
    where alphs = (snd $ head $ filter ((== tok) . fst) keyPad)

keyPad :: [(Token, String)]
keyPad = [('1',""),('2',"abc"),('3',"def"),('4',"ghi"),('5',"jkl"),('6',"mno"),
         ('7',"pqrs"),('8',"tuv"),('9',"wxyz"),('*',"^"),('0',"+ "), ('#',"#.,?!")]






-- note returns just one set of puttons - not for whole sentence
tokenToButton :: Token -> ButtonGroup
tokenToButton tok
    | isUpper tok = Uppercase : Letter (toLower tok) : []
    | isLower tok = Lowercase : Letter tok : []
    | isDigit tok = Number tok : []
    | isSpace tok = Spacebar : []
    | isSign tok  = Sign tok : []
    | otherwise = Unknown : []
    where isSign t = elem t "+#.,?!"

tokenToFinger :: Token -> FingerMoveGroup
tokenToFinger tok
    | isUpper tok = ('*',1) : tokenWithPresses tok : []
    | isLower tok = tokenWithPresses tok : []
    | isDigit tok = (tok, 1) : []
    | isSpace tok = ('0', 2) : []
    | isSign tok  = ('*',2) : tokenWithPresses tok : []
    | otherwise   = []
    where isSign t = elem t "+#.,?!"


-- note converts one letter/num/sign worth of buttons into finger moves.
-- note expects only one single digit at a time. If it's not a single digit, then
-- the result char is not going to be the char form of the digit.
buttonToFingers :: ButtonGroup -> FingerMoveGroup
buttonToFingers [Uppercase, Letter n] = ('*',1) : tokenWithPresses n : []
buttonToFingers [Lowercase, Letter n] = tokenWithPresses n : []
buttonToFingers [Number n] = (n, 1) : []
buttonToFingers [Spacebar] = ('0',2) : []


-- note
{-

fingerToButton :: FingerMoves -> ButtonGroup
fingerToButton (('*',2) : (c,1) : rest) -- note extracting digit case (where p==1)
                    = NumPad : Number c : fingerToButton rest
fingerToButton (('*',2) : (c,p) : rest)
        = EngPad : Lowercase : Letter (tokenFromPresses (c,p)) : fingerToButton rest
-}


-- only expects the Token present in the phonepad: letters,nums, *+-^#,.'?!
-- note expect list length no greater than 2.





{-
Inspiration links:

https://github.com/Tclv/HaskellBook/blob/master/ch11/Phone.hs
https://github.com/dwayne/haskell-programming/blob/master/ch11/Phone.hs
https://github.com/vaughanj10/haskell_programming/blob/master/ch11/phone.hs
https://github.com/juank-pa/haskell-training/blob/master/Chapter11/Exercises/DaPhone.hs

-}