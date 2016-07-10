import Data.Char

data Option = Digit Char | Capitalize deriving (Eq, Show)

data Button = Button ButtonIdentifier [Option] deriving (Eq, Show)

data ButtonIdentifier = One | Two | Three | Four
                      | Five | Six | Seven | Eight
                      | Nine | Star | Zero | Bracket
                      deriving (Eq, Show)

type Presses = Int

newtype PhonePad = PhonePad [Button] deriving (Eq, Show)

phonePad :: PhonePad
phonePad = PhonePad [one, two, three
                    ,four, five, six
                    ,seven, eight, nine
                    ,star, zero, bracket]
  where
    one = Button One (buildDigitList ['1'])
    two = Button Two (buildDigitList ['a', 'b', 'c', '2'])
    three = Button Three (buildDigitList ['d', 'e', 'f', '3'])
    four = Button Four (buildDigitList ['g', 'h', 'i', '4'])
    five = Button Five (buildDigitList ['j', 'k', 'l', '5'])
    six = Button Six (buildDigitList ['m', 'n', 'o', '6'])
    seven = Button Seven (buildDigitList ['p', 'q', 'r', 's', '7'])
    eight = Button Eight (buildDigitList ['t', 'u', 'v', '8'])
    nine = Button Nine (buildDigitList ['w', 'x', 'y', 'z', '9'])
    star = Button Star [Capitalize]
    zero = Button Zero (buildDigitList ['+', ' ', '0'])
    bracket = Button Bracket (buildDigitList ['.', ',', '\n'])


buildDigitList :: [Char] -> [Option]
buildDigitList = map Digit


reversePhonePad (PhonePad buttons) = lookUpify buildList
  where
    lookUpify = map (\(x, y, z) -> (x, (y, z)))
    buildList = concat $ map (\(Button btype options) -> zip3 options (repeat btype) [1..]) buttons


onSepLines :: Show a => [a] -> IO()
onSepLines {-xs-} = putStr . concatMap ((++ "\n") . show) {-xs-}