
data Move = Rock | Paper | Scissors deriving (Eq, Show)

data Result = Win | Lose | Draw deriving (Eq, Show)

type Tournament = ([Move], [Move]) -- moves made by two players



beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat _ = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose _ = Paper

outcome :: Move -> Move -> Integer
outcome Rock Scissors     = 1
outcome Paper Rock        = 1
outcome Scissors Paper    = 1
outcome Rock Paper        = -1
outcome Paper Scissors    = -1
outcome Scissors Rock     = -1
outcome Rock Rock         = 0
outcome Paper Paper       = 0
outcome Scissors Scissors = 0



-- exercise 1 --------------------------------------------------------------------------
-- precondition - the lists must be same size

t1, t2, t3, t4, t5 :: Tournament
t1 = ([Rock, Rock,Paper],[Scissors,Paper,Rock])
t2 = ([Scissors, Rock,Paper],[Scissors,Rock,Paper])
t3 = ([Paper, Rock, Paper],[Scissors, Paper, Rock])
t4 = ([Scissors, Paper, Rock],[Rock, Scissors, Paper])
t5 = ([Rock, Scissors, Paper], [Scissors, Paper, Rock])



tournamentOutcome :: Tournament -> Integer
tournamentOutcome ([], []) = 0
tournamentOutcome (as, bs) = sum [outcome a b | (a,b) <- zip as bs]

