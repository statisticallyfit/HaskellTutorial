module Phone where

import Data.Char
import Data.List
import Data.Maybe



type Token = Char
type Text = [[Token]]
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
            deriving (Eq, Show)

type ButtonGroup = [Button] -- note a group of buttons for 1 letter/num/sign

data Phone = PhonePad [Button] deriving (Eq, Show)



phone :: IO()
phone = putStr $ break ++ row123 ++ break ++ row456 ++ break ++
                   row789 ++ break ++ rowOp ++ break
    where break  = "-------------------------------------\n"
          row123 = "|   1       |   2 ABC   |   3 DEF   |\n"
          row456 = "|   4 GHI   |   5 JKL   |   6 MNO   |\n"
          row789 = "|   7 PQRS  |   8 TUV   |   9 WXYZ  |\n"
          rowOp  = "|   * ^     |   0 + _   |   # .,?!  |\n"

-- note do not switch order of (0,+) tuple and the (*, ^) tuple because we need
-- to find spacebar in the 0 tuple and not in the * tuple. Made spacebar next to ^ in
-- the * tuple so that pressing * twice gives the exp ^ char.
keyPad :: [(Token, String)]
keyPad = [('1',"1"),('2',"abc2"),('3',"def3"),('4',"ghi4"),('5',"jkl5"),
          ('6',"mno6"), ('7',"pqrs7"),('8',"tuv8"),('9',"wxyz9"),('0',"+ 0"),
          ('*'," ^"),('#',"#.,?!")]




-- todo fix this over here oki doki?
{-
RULE key: ------------------------------------------------------------------------------
Whenever there is a number in front during letter format, we ignore it
and start counting presses from second non-number element (see '+' and ' ' below)

presses example 'e' = ('3', 2) which means press 3 twice.
* --> capitalize -> ('*',1) -- note repeat if we need multiple capitalizers in row.
^ -> exponent char -> ('*',2)
0 -> spacebar -> ('0',2)
+ -> plus sign -> ('0', 1)
? -> questionmark -> ('#', 4)
The only symbols that exist are: space + . , ? ! # ^
because * is taken for capitalization.
-}



------------------------------------------------------------------------------------------


-- postcondition returns simple count of presses - does not consider switching pads
-- nor capitals.
presses :: Token -> Presses
presses tok = 1 + head (catMaybes (map (elemIndex (toLower tok)) (map snd keyPad)))

isSign :: Token -> Bool
isSign tok = elem tok "+#.,?!"

-- postcondition returns finger equivalent of token - does not count switching
-- pads nor capitals.
ravel :: Token -> FingerMove
ravel tok = (fst $ head $ filter ((elem tok') . snd) keyPad, presses tok')
    where tok' = toLower tok

-- postcondition inverse of ravel - returns token from finger move - does not account for
-- switching pads nor capitals
-- note it is incorrect but unravel ('*',1) returns ' ' (spacebar) but that is supposed
-- to be the result for ('0',2) only.
unravel :: FingerMove -> Token
unravel (c,p) = alphas !! (p - 1)
    where alphas = (snd $ head $ filter ((== c) . fst) keyPad)


------------------------------------------------------------------------------------------

-- NOTE The communication functions



-- note returns sentence's worth of buttons.
fingerButtonize :: [FingerMove] -> [Button]
fingerButtonize [] = []
fingerButtonize (('*',1) : (c,p) : fngs)
    = (CapitalLetter $ unravel (c,p)) : fingerButtonize fngs
fingerButtonize ((c,p) : fngs)
    | isSign sym = Sign sym : fingerButtonize fngs
    | isLetter sym = Letter sym : fingerButtonize fngs
    | isNumber sym = Number sym : fingerButtonize fngs
    | isSpace sym = Spacebar : fingerButtonize fngs
    where sym = unravel (c,p)


fingerTokenize :: [FingerMove] -> [Token]
fingerTokenize = buttonTokenize . fingerButtonize


tokenButtonize :: [Token] -> [Button]
tokenButtonize [] = []
tokenButtonize (tok : tokens)
    | isUpper tok = CapitalLetter (toLower tok) : tokenButtonize tokens
    | isLower tok = Letter tok : tokenButtonize tokens
    | isNumber tok = Number tok : tokenButtonize tokens
    | isSpace tok = Spacebar : tokenButtonize tokens
    | isSign tok = Sign tok : tokenButtonize tokens


tokenFingerize :: [Token] -> [FingerMove]
tokenFingerize = buttonFingerize . tokenButtonize


buttonTokenize :: [Button] -> [Token]
buttonTokenize [] = []
buttonTokenize (CapitalLetter n : buttons) = (toUpper n) : buttonTokenize buttons
buttonTokenize (Letter n : buttons) = n : buttonTokenize buttons
buttonTokenize (Number n : buttons) = n : buttonTokenize buttons
buttonTokenize (Spacebar : buttons) = ' ' : buttonTokenize buttons
buttonTokenize (Sign n : buttons) = n : buttonTokenize buttons


-- note converts one letter/num/sign worth of buttons into finger moves.
-- note expects only one single digit at a time. If it's not a single digit, then
-- the result char is not going to be the char form of the digit.
buttonFingerize :: [Button] -> [FingerMove]
buttonFingerize [] = []
buttonFingerize (CapitalLetter n : btns) = ('*',1) : ravel n : buttonFingerize btns
buttonFingerize (Letter n : btns) = ravel n : buttonFingerize btns
buttonFingerize (Sign n : btns)   = ravel n : buttonFingerize btns
buttonFingerize (Number n : btns) = (n, presses n) : buttonFingerize btns
buttonFingerize (Spacebar : btns) = ('0',2) : buttonFingerize btns






---------------------------------------------------------------------------------------

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



-- note returns cost of a particular token (num presses).
cost :: Token -> Presses
cost token = if isUpper token then 1 + presses token else presses token

-- note returns  num times the token appears in the text times its cost for one press.
costIn :: Token -> Text -> Presses
costIn tkn txt = cost tkn * appears tkn txt


-- note returns number of occurrences of tkn in txt.
appears :: Token -> Text -> Int
appears tkn txt = length $ elemIndices tkn (concat txt)


-- note gets the most popular letter throughout the whole conversation.
coolestLetter :: Text -> Char
coolestLetter = mostPopularLetter . concat


-- note gets most oftenest word, ignores signs, numbers.
coolestWord :: Text -> String
coolestWord txt = fst $ occs !! indexOfMax
    where isEng c = isSpace c || isLetter c
          txt' = map (filter isEng) txt
          ws = concat $ map words txt'
          wordOcc ws w = (w, length $ elemIndices w ws)
          occs = (map $ wordOcc ws) ws
          maxOcc = maximum $ map snd occs
          indexOfMax = fromJust $ findIndex ((== maxOcc) . snd) occs


-- note returns the first rarest word.
rarestWord :: Text -> String
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

convo :: Text
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

text :: Text
text =
    ["The night sky is littered with 1000s of stars.",
     "40 Swallows sing in the grey dawn.",
     "Morning dew settles on lilac bushes",
     "Fog on the lake rises in golden splendor.",
     "Traced frozen lava scoured by 10 winds.",
     "Glowing crystal caves.",
     "+ #,.?!123abc123..?.abc"] -- tests switching


hmgbd :: Text
hmgbd = ["Hummingbirds swoop down from their perches",
         "and hummingbirds race to the spring",
         "Hummingbirds feed on seeds,",
         "hummingbirds sing"]


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

{-phone :: Phone
phone = PhonePad $ concat [numbers, lowLetters, uppLetters, signs, space]
    where numbers = map (Number $) (concatMap show [0..9])
          lowLetters = map (Letter $) ['a' .. 'z']
          uppLetters = map (CapitalLetter $) ['a' .. 'z']
          signs = map (Sign $) "+.,?!#"
          space = [Spacebar]-}
