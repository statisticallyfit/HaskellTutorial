module Phone where

import Data.Char
import Data.List
import Data.Maybe



type Token = Char
type Presses = Int -- >= 1
type FingerMove = (Token, Presses)

-- note distinguishing between uppercase and lower case so there is no more
-- need to check in functions if the arg Token in Button is lower or uppercase.
-- note SwitchFormat refers to switching from letters + punctuation to numbers
-- and vice versa. If before was nums, then say Switch, then switch to english.
-- note NumPad lasts until EngPad is specified. Rest that follow are in that format.
-- example: To Swtich from capital to lower press * after a capital and vice versa.
data Button = NumPad | EngPad{- Switch -}| Spacebar
            | Number Token
            | CapitalLetter Token
            | Letter Token
            | Sign Token
            | Unknown -- for symbols outside space + . , ? ! #
            deriving (Eq, Show)

type ButtonGroup = [Button] -- note a group of buttons for 1 letter/num/sign

data Phone = PhonePad [Button] deriving (Eq, Show)



phone :: Phone
phone = PhonePad $ concat [numbers, lowLetters, uppLetters, signs, space]
    where numbers = map (Number $) (concatMap show [0..9])
          lowLetters = map (Letter $) ['a' .. 'z']
          uppLetters = map (CapitalLetter $) ['A' .. 'Z']
          signs = map (Sign $) "+.,?!#"
          space = [Spacebar]


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
^ -> switchformat -> ('*',2)

Letter format: ---------------------------------------------------------------------
RULE key: whenever there is a number in front during letter format, we ignore it
and start counting presses from second non-number element (see '+' and ' ' below)

presses example 'e' = ('3', 2) which means press 3 twice.
* --> capitalize -> ('*',1) -- note repeat if we need multiple capitalizers in row.
^ -> switch format -> ('*',2)
0 -> spacebar -> ('0',2)
+ -> plus sign -> ('0', 1)
? -> questionmark -> ('#', 4)
The only symbols that exist are: space + . , ? ! #
-}
-- note only expects lowercase letters
-- throws error if given upppercase or if given numbers.
countKey :: Token -> Presses
countKey tok = 1 + head (catMaybes (map (elemIndex tok) (map snd keyPad)))

isSign :: Token -> Bool
isSign tok = elem tok "+#.,?!"

ravel :: Token -> FingerMove
ravel tok = (fst $ head $ filter ((elem (toLower tok)) . snd) keyPad,
                                      countKey (toLower tok))

unravel :: FingerMove -> Token
unravel (c,p) = alphas !! (p - 1)
    where alphas = (snd $ head $ filter ((== c) . fst) keyPad)

keyPad :: [(Token, String)]
keyPad = [('1',""),('2',"abc"),('3',"def"),('4',"ghi"),('5',"jkl"),
          ('6',"mno"), ('7',"pqrs"),('8',"tuv"),('9',"wxyz"),('*',"^"),
          ('0',"+ "), ('#',"#.,?!")]




getToken :: Button -> Maybe Token
getToken (Number n) = Just n
getToken (Sign n) = Just n
getToken (Letter n) = Just n
getToken (CapitalLetter n) = Just $ toUpper n
getToken (Spacebar) = Just ' '
getToken _ = Nothing


-- Lowercase or Uppercase = 1 press to press '*' char
-- letter == depends on its location
-- spacebar == press 0 two times.
getPresses :: Button -> Maybe Presses
getPresses (Letter n) = Just $ countKey n
getPresses (CapitalLetter n) = Just $ 1 + countKey n
getPresses (Sign n) = Just $ countKey n
getPresses (Number _) = Just 1
getPresses (Spacebar) = Just 2 -- press 0 two times. (knowing it is LETTER FORMAT)
getPresses _ = Nothing


-- note returns just one set of puttons - not for whole sentence
tokenButtonize :: [Token] -> [Button]
tokenButtonize [] = []
tokenButtonize tokens = concatMap classify engNums
    where engNums = splitEngNums tokens
          classify tokPiece
            | isNumber $ head tokPiece = NumPad : (map tokToBtn tokPiece)
            | otherwise = EngPad : (map tokToBtn tokPiece)
          tokToBtn tok
            | isUpper tok = CapitalLetter (toLower tok)
            | isLower tok = Letter tok
            | isDigit tok = Number tok
            | isSpace tok = Spacebar
            | isSign tok  = Sign tok
            | otherwise = Unknown


--tokenFingerize :: [Token] -> [FingerMove]
--tokenFingerize tokens = buttonFingerize $ tokenButtonize tokens

-- note converts one letter/num/sign worth of buttons into finger moves.
-- note expects only one single digit at a time. If it's not a single digit, then
-- the result char is not going to be the char form of the digit.
buttonFingerize :: [Button] -> [FingerMove]
buttonFingerize [] = []
buttonFingerize (CapitalLetter n : btns) = ('*',1) : ravel n : buttonFingerize btns
buttonFingerize (Letter n : btns) = ravel n : buttonFingerize btns
buttonFingerize (Sign n : btns)   = ravel n : buttonFingerize btns
buttonFingerize (Number n : btns) = (n, 1) : buttonFingerize btns
buttonFingerize (Spacebar : btns) = ('0',2) : buttonFingerize btns
buttonFingerize (Unknown : _)    = []


-- note converts sentence worth of buttons into tokens.
buttonTokenize :: [Button] -> [Token]
buttonTokenize [] = []
buttonTokenize (CapitalLetter n : btns) = toUpper n : buttonTokenize btns
buttonTokenize (Letter n : btns) = n : buttonTokenize btns
buttonTokenize (Number n : btns) = n : buttonTokenize btns
buttonTokenize (Sign n : btns) = n : buttonTokenize btns
buttonTokenize (Spacebar : btns) = ' ' : buttonTokenize btns
buttonTokenize (Unknown : _) = undefined
buttonTokenize (_ : btns) = buttonTokenize btns


--fingersButtonize :: [FingerMove] -> [Button]


--fingersTokenize :: [FingerMove] -> [Token]
-- HELP it's spouting numbers in the middle of words - fix tokenButtonize with EngPad
fingersTokenize [] = []
fingersTokenize (('*',2):('*',1):(c,p):taps) = (toUpper $ unravel (c,p)) : fingersTokenize taps
fingersTokenize (('*',2):(c,1):taps) = c : fingersTokenize taps
fingersTokenize (('*',2):(c,p):taps) = unravel (c,p) : fingersTokenize taps
fingersTokenize (('*',1):(c,p):taps) = (toUpper $ unravel (c,p)) : fingersTokenize taps
fingersTokenize ((c,1):taps) = c : fingersTokenize taps
fingersTokenize ((c,p):taps) = unravel (c,p) : fingersTokenize taps



-- fing - button - token
--fingerToToken :: [FingerMove] -> Token
{-
fingerToToken fingMoves
    | (head fingMoves) == ('*',1) = toUpper convertedToken
    | otherwise = convertedToken
    where (c,p) = last fingMoves
          alphas = (snd $ head $ filter ((== c) . fst) keyPad)
          convertedToken = alphas !! (p - 1)
-}

-- fing - button
{-fingersToButtons :: [FingerMove] -> [Button]
fingersToButtons [] = []
fingersToButtons (('*',2) : rest) = Switch : fingersToButtons rest
fingersToButtons ((c,1) : rest) = Number c : fingersToButtons rest
--fingersToButtons (('*',1) : rest) = CapitalLetter tok : fingersToButtons rest
fingersToButtons ((c,p) : rest) = Letter tok : fingersToButtons rest
    where --(c,p) = head rest
          alphas = (snd $ head $ filter ((== c) . fst) keyPad)
          tok = alphas !! (p - 1)-}
{-
fingerToButton (('*',2) : ('*',1) : rest : [])
                = EngPad : CapitalLetter tok : fingerToButton rest
fingerToButton (('*',2) : (c,1) : rest : [])
                = NumPad : Number c : fingerToButton rest
fingerToButton (('*',2) : (c,p) : rest : [])
                = EngPad : Letter tok : fingerToButton rest
fingerToButton


-}
---------------------------------------------------------------
-- Now for the actual conversation translators


-- postcondition: returns english separated from numbers.
-- splitEngNums "a1b2c3d4f"   --- >    ["a","1","b","2","c","3","d","4","f"]
-- splitEngNums "abc!#+.,+12!!3de!!f"   --- >    ["abc!#+.,+","12","!!","3","de!!f"]
splitEngNums :: [Token] -> [[Token]]
splitEngNums [] = []
splitEngNums [t] = [[t]]
splitEngNums ts@(fstTok : tokens)
     = [takeRuns ts] ++ splitEngNums (dropRuns ts)
    where takeRuns ts
            | isNumber fstTok = takeWhile isNumber ts
            | otherwise = takeWhile (not . isNumber) ts
          dropRuns ts
            | isNumber fstTok = dropWhile isNumber ts
            | otherwise = dropWhile (not . isNumber) ts







text :: [String]
text =
    ["The night sky is littered with 1000s of stars.",
     "40 Swallows sing in the grey dawn.",
     "Morning dew settles on lilac bushes",
     "Fog on the lake rises in golden splendor.",
     "Traced frozen lava scoured by 10 winds.",
     "Glowing crystal caves.",
     "+ #,.?!123abc123..?.abc"] -- tests switching

{-

encodeConversation :: [String] -> [[[FingerMove]]]
encodeConversation convo = map (map tokenToFinger) convo

decodeConversation :: [[[FingerMove]]] -> [String]
decodeConversation fmss = map (map fingerToToken) fmss
-}



{-
Inspiration links:

https://github.com/Tclv/HaskellBook/blob/master/ch11/Phone.hs
https://github.com/dwayne/haskell-programming/blob/master/ch11/Phone.hs
https://github.com/vaughanj10/haskell_programming/blob/master/ch11/phone.hs
https://github.com/juank-pa/haskell-training/blob/master/Chapter11/Exercises/DaPhone.hs

-}





--- nice scanl use:

-- postcondition: puts Engpad when we have english and NumPad when we have numbers.
{-switchPad :: [Token] -> [Button]
switchPad tokens = tail $ scanl detectShift EngPad tokens
    where isEnglish y = isLetter y || isSign y
          detectShift acc y
            | isNumber y   = NumPad
            | isEnglish y  = EngPad
-}