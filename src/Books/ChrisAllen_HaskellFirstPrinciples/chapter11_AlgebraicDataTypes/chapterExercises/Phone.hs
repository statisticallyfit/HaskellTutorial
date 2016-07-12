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



------------------------------------------------------------------------------------------

-- NOTE the utility functions


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


tokLabels :: [(Button, [FingerMove])] -> [Token]
tokLabels [] = []
tokLabels ((NumPad, tps) : rest) = (map fst tps) ++ tokLabels rest
tokLabels ((EngPad, tps) : rest)
    | noCapitals tps = (map unravel tps) ++ tokLabels rest
    | otherwise = parseCapitals tps ++ tokLabels rest
    where noCapitals ts = (length $ filter (== ('*',1)) ts) == 0


parseCapitals :: [FingerMove] -> [Token]
parseCapitals [] = []
parseCapitals (('*',1) : (c,p) : rest) = (toUpper $ unravel (c,p)) : parseCapitals rest
parseCapitals ((c,p) : rest) = (unravel (c,p)) : parseCapitals rest


labelTaps :: [FingerMove] -> [(Button, [FingerMove])]
labelTaps taps = map label identifiedTaps
    where identifiedTaps = zip (map isNumChunk (chunk taps)) (chunk taps)
          label (b, tps)
            | b = (NumPad, tail tps)
            | otherwise = (EngPad, tail tps)

chunk :: [FingerMove] -> [[FingerMove]]
chunk [] = []
chunk taps
    | head taps == switch = [switch : runs (tail taps)] ++ chunk (rest (tail taps))
    | otherwise = [runs taps] ++ chunk (rest taps)
    where switch = ('*',2)
          runs ts = takeWhile (/= switch) ts
          rest ts = dropWhile (/= switch) ts

isNumChunk :: [FingerMove] -> Bool
isNumChunk [] = False
isNumChunk taps
    | length taps == 1 = if ((snd $ head taps) == 1) then True else False
    | head taps == switch = isNumChunk (tail taps)
    | otherwise = and $ map ((== 1) . snd) taps
    where switch = ('*',2)

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


------------------------------------------------------------------------------------------

-- NOTE The communication functions

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
getPresses Spacebar = Just 2 -- press 0 two times. (knowing it is LETTER FORMAT)
getPresses EngPad = Just 2 -- since we press ('*',2)
getPresses NumPad = Just 2
getPresses Unknown = Nothing


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


tokenFingerize :: [Token] -> [FingerMove]
tokenFingerize tokens = buttonFingerize $ tokenButtonize tokens


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
buttonFingerize (_ : btns) = ('*',2) : buttonFingerize btns


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



fingerTokenize :: [FingerMove] -> [Token]
fingerTokenize taps = tokLabels (labelTaps taps)


fingerButtonize :: [FingerMove] -> [Button]
fingerButtonize taps = tokenButtonize $ fingerTokenize taps



-- note sums the taps for each finger move
totalTaps :: [FingerMove] -> Presses
totalTaps = sum . catMaybes . map getPresses . fingerButtonize
-- or sum .  map snd

-- note returns the first most popular letter (letter that occurs most often).
-- if all letters are different or have a tie, returns first letter that has a tie.
-- note ignores any non-letters.
mostPopularLetter :: String -> Token
mostPopularLetter txt = fst $ occs !! indexOfMax
    where txt' = map toLower (filter isLetter txt)
          letterOcc xs x = (x, length $ elemIndices x xs)
          occs = (map $ letterOcc txt') txt'
          maxOcc = maximum $ map snd occs
          indexOfMax = fromJust $ findIndex ((== maxOcc) . snd) occs


-- note gets num taps required to press a given token.
cost :: Token -> Presses
cost token = totalTaps $ tokenFingerize [token]


-- note gets the most popular letter throughout the whole conversation. [String]
coolestLetter :: [[Token]] -> Char
coolestLetter = mostPopularLetter . concat


-- note gets most oftenest word, ignores signs, numbers.
coolestWord :: [[Token]] -> String
coolestWord txt = fst $ occs !! indexOfMax
    where isEng c = isSpace c || isLetter c
          txt' = map (filter isEng) txt
          ws = concat $ map words txt'
          wordOcc ws w = (w, length $ elemIndices w ws)
          occs = (map $ wordOcc ws) ws
          maxOcc = maximum $ map snd occs
          indexOfMax = fromJust $ findIndex ((== maxOcc) . snd) occs


-- note returns the first rarest word.
rarestWord :: [[Token]] -> String
rarestWord txt = fst $ occs !! indexOfMin
    where isEng c = isSpace c || isLetter c
          txt' = map (filter isEng) txt
          ws = concat $ map words txt'
          wordOcc ws w = (w, length $ elemIndices w ws)
          occs = (map $ wordOcc ws) ws
          minOcc = minimum $ map snd occs
          indexOfMin = fromJust $ findIndex ((== minOcc) . snd) occs



---------------------------------------------------------------

-- NOTE Now for the actual conversation translators

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted cotton candy lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

text :: [String]
text =
    ["The night sky is littered with 1000s of stars.",
     "40 Swallows sing in the grey dawn.",
     "Morning dew settles on lilac bushes",
     "Fog on the lake rises in golden splendor.",
     "Traced frozen lava scoured by 10 winds.",
     "Glowing crystal caves.",
     "+ #,.?!123abc123..?.abc"] -- tests switching



encrypt :: [[Token]] -> [[FingerMove]]
encrypt convo = map tokenFingerize convo

decrypt :: [[FingerMove]] -> [[Token]]
decrypt fmss = map fingerTokenize fmss




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