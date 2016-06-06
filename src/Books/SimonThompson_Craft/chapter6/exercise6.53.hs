
data Suit = Spades | Hearts | Diamonds | Clubs deriving (Eq, Ord, Show, Enum)

data Value = Two | Three | Four | Five | Six | Seven | Eight |
             Nine | Ten | Jack | Queen | King | Ace
             deriving (Eq, Ord, Show, Enum)

data Card = Card Suit Value deriving (Eq, Ord, Show{-, Enum-}) -- help why error when Enum?

type Deck = [Card]

data Player = North | South | East | West deriving (Eq, Ord, Show, Enum)

-- note need to know which player was the lead, so use list. Lead is first in the list.
type Trick = [(Player, Card)]



deck :: Deck
deck = [Card suit value | suit <- [Spades .. Clubs], value <- [Two .. Ace]]



displayDeck      :: Deck -> IO()
displayDeck deck = putStrLn $ concat [show value ++ " of " ++ show suit ++ "\n"
                    | (Card suit value) <- deck]


-- exercise 57 --------------------------------------------------------------------------

suitLead :: Trick -> Suit
suitLead trick = s   -- returns the suit of the lead player
                 where (Card s v) = snd $ head trick

-- note: says who won the trick assuming no trump suit, meaning first suit is trumpSuit
winNT :: Trick -> Player
winNT trick = winT (suitLead trick) trick



-- exercise 58 --------------------------------------------------------------------------

-- precondition - none since we check whether there is a trump suit anyway. If there
-- is no trump suit of the one we want then return the lead suit.
-- had help here
winT :: Suit -> Trick -> Player
winT trumpSuit trick = playerOfHighestTrump
        where hasTrumps = length [p| (p, Card s v) <- trick, s == trumpSuit] > 0
              trumpSuit' = if hasTrumps then trumpSuit else suitLead trick
              maxTrumpValue = maximum [v | (p, Card s v) <- trick, s == trumpSuit']
              playerOfHighestTrump = head [p | (p, Card s v) <- trick, v == maxTrumpValue]



-- exercise 59 --------------------------------------------------------------------------

type Hand = (Player, [Card]) -- collection of cards held by one player at any point.

-- exercise 60 --------------------------------------------------------------------------

type Hands = [Hand]

-- exercise 61 --------------------------------------------------------------------------


-- note: rules - once a certain player runs out of a suit, he or she cannot go back
-- to using that suit. So assume there are suit runs for each particular player.
-- precondition: assume that if player B plays a trump, then player C should still
-- play the suit of the card that went before B, if C has it.


t1, t2, t3, t4, t5, t6 :: Trick

-- possible && legal
t1 = [(North, Card Hearts Queen),
      (East, Card Hearts Two),
      (South, Card Hearts Three),
      (West, Card Hearts King)]


-- Not possible - since North does not have hearts of seven
-- Not legal - since South had Hearts Three but did not use it.
t2 = [(North, Card Hearts Seven),
      (East, Card Hearts Two),
      (South, Card Clubs Jack),
      (West, Card Hearts King)]

-- Is possible -- all cards shown belong in the respective players' hands
-- not legal -- East doesn't follow suit, should have chosen Hearts Two
t3 = [(North, Card Hearts Queen),
      (East, Card Spades Five),
      (South, Card Clubs Three),
      (West, Card Diamonds Ace)]

-- possible && legal
t4 = [(North, Card Spades Six),
      (East, Card Spades Two),
      (South, Card Clubs Ten),
      (West, Card Diamonds Four)]

-- Not legal - East doesn't follow suit (has Spades but won't use it, uses another
-- card
t5 = [(North, Card Spades Six),
      (East, Card Diamonds Jack),
      (South, Card Clubs Ten),
      (West, Card Diamonds Four)]


-- Not possible -- East has hearts Two but won't use it , makes up another card
t6 = [(North, Card Hearts Queen),
      (East, Card Hearts King),
      (South, Card Hearts Three),
      (West, Card Hearts King)]

-- possible && legal - carries out precondition at beginning. Here East and West
-- are sharing the same cards so could be said to be competing or not
t7 = [(North, Card Clubs Three),
      (East, Card Diamonds Jack),
      (South, Card Clubs Jack),
      (West, Card Diamonds Ace)]

-- possible && legal - each member of each team has different cards, so must count
-- as trumped
t8 = [(North, Card Spades Six),
      (East, Card Spades Five),
      (South, Card Clubs Jack),
      (West, Card Diamonds Ace) ]



hands :: Hands
hands = [ (North, [Card Hearts Queen, Card Hearts Ace, Card Spades Six,
                   Card Clubs Three]),
          (East, [Card Hearts Two, Card Spades Five, Card Spades Two,
                  Card Diamonds Jack]),
          (South, [Card Clubs Jack, Card Clubs Three, Card Clubs Ten,
                   Card Hearts Three]),
          (West, [Card Hearts King, Card Diamonds Ace, Card Diamonds Four,
                  Card Diamonds Nine])
        ]

northHand :: Hand
northHand = (North, [Card Hearts Queen, Card Hearts Ace, Card Spades Six,
                     Card Clubs Three])
southHand :: Hand
southHand = (South, [Card Clubs Jack, Card Clubs Three, Card Clubs Ten,
                     Card Hearts Three])
eastHand :: Hand
eastHand = (East, [Card Hearts Two, Card Spades Five, Card Spades Two,
                     Card Diamonds Jack])



-- note checks if this particular card is in a player's hand
isCard :: Card -> Hand -> Bool
isCard searchCard hand = length [c | c <- cardList, c == searchCard] /= 0
                         where cardList = snd hand

-- note checks if this particular suit is in a player's hand
isSuit :: Suit -> Hand -> Bool
isSuit searchSuit hand = length [s | (Card s v) <- cardList, s == searchSuit] /= 0
                         where cardList = snd hand

-- note: given the player put this suit in the trick, did he/she play possibly?
-- Means check if suit is in his/her hand
possible :: Hand -> Card -> Bool
possible hand cardPlayed = isCard cardPlayed hand

-- note checks if the player played legally.
-- Means: if the firstSuit in trick is not equal to suit of player, then we must
-- investigate. Next if firstSuit is inside the hand, then player could have played it
-- so he fooled us to it's illegal. Take oppposite to make it legal.
legal :: Hand -> Suit -> Suit -> Bool
legal hand suitPlayed suitFirst = not ((suitFirst /= suitPlayed) &&
                                          (isSuit suitFirst hand))


-- note -- returns the hand of a particular player, given a "hands"
playerHand :: Player -> Hands -> Hand
playerHand player hands = head [(p, c) | (p, c) <- hands, p == player]

-- note returns the suit played by a particular player in a trick
playerSuit :: Player -> Trick -> Suit
playerSuit player trick = head [s | (p, Card s v) <- trick, p == player]

-- note returns the card played by a particular player in a trick
playerCard :: Player -> Trick -> Card
playerCard player trick = head [c | (p, c) <- trick, p == player]

-- note check whether trick is possible and legal
-- possible: card of each player from the trick should be in their hand, as given in hands
-- legal: players follow suit, so cannot trump if you have the correct suit.
-- precondition: assume that whenever we compare the legality, we do it with firstSuit.
-- the next suit does not become the comparator even if the suits change.
checkPlay :: Hands -> Trick -> Bool
checkPlay hs t = allLegal && allPossible
    where firstSuit = suitLead t
          allLegal = and [legal (playerHand p hs) (playerSuit p t) firstSuit
                         | (p,c) <- t]
          allPossible = and [possible (playerHand p hs) (playerCard p t)
                            | (p,c) <- t ]








-- exercise 62 -------------------------------------------------------------------------

data Team = NorthSouth | EastWest deriving (Eq, Show)

-- assume - no need for 13 tricks

winnerNT :: [Trick] -> Team
winnerNT trickList = [winNT trick | trick <- trickList]


{-
winNT :: Trick -> Player
winNT trick = winT (suitLead trick) trick

winT :: Suit -> Trick -> Player
winT trumpSuit trick = playerOfHighestTrump
        where hasTrumps = length [p| (p, Card s v) <- trick, s == trumpSuit] > 0
              trumpSuit' = if hasTrumps then trumpSuit else suitLead trick
              maxTrumpValue = maximum [v | (p, Card s v) <- trick, s == trumpSuit']
              playerOfHighestTrump = head [p | (p, Card s v) <- trick, v == maxTrumpValue]
-}
