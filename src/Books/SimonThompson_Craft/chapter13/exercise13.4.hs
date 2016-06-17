
data Move = Rock | Paper | Scissors deriving (Eq, Show)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine
            | Ten | Jack | Queen | King | Ace deriving (Eq, Show)

data Suit = Hearts | Diamonds | Spades | Clubs deriving (Eq, Show)

data Card = Card Value Suit deriving (Eq, Show)



class Info a where
    examples :: [a]
    size     :: a -> Int




-- Move
instance Info Move where
    examples = [Rock, Paper, Scissors]
    size _   = 1

-- Cards
instance Info Card where
    examples = [Card Ace Spades, Card Seven Clubs, Card Queen Hearts, Card Nine Diamonds]
    size _   = 2

-- Triple values
instance Info Char where
    examples = ['a', 'A', 'z', 'Z', '0', '9']
    size _   = 1

instance (Info a, Info b, Info c) => Info (a,b,c) where
    examples     = [(x,y,z) | x <- examples, y <- examples, z <- examples]
    size (x,y,z) = size x + size y + size z + 1






main = do
    print $ map size (replicate 2 ('a','b','c'))
    print $ foldr (+) 1 (map size (replicate 2 ('a','b','c')))