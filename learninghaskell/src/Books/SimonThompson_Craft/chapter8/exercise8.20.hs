import Data.Text (justifyLeft, pack, unpack)

data Move = Rock | Paper | Scissors deriving (Eq, Show)

data Result = Win | Lose | Draw deriving (Eq, Show)

type Tournament = ([Move], [Move]) -- moves made by two players

type Strategy = [Move] -> Move -- note next move depends on previous opponent moves.

----------------------------------------------------------------------------------------
tournamentOutcome :: Tournament -> Integer
tournamentOutcome ([], []) = 0
tournamentOutcome (as, bs) = sum [outcome a b | (a,b) <- zip as bs]


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


-----------------------------------------------------------------------------------------
-- STRATEGIES


frequencies :: [Move] -> (Int, Int, Int) -> (Int, Int, Int)
frequencies [] (rc, pc, sc) = (rc, pc, sc)
frequencies (m:ms) (rc, pc, sc)
    | m == Rock      = frequencies ms (rc + 1, pc, sc)
    | m == Paper     = frequencies ms (rc, pc + 1, sc)
    | otherwise      = frequencies ms (rc, pc, sc + 1)

beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat _ = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose _ = Paper


echo :: Strategy
echo (latest : rest) = latest
echo []              = Rock


beatLastMove :: Strategy
beatLastMove []     = Rock -- arbitrary choice
beatLastMove (m:ms) = beat m

loseLastMove :: Strategy
loseLastMove []     = Scissors
loseLastMove (m:ms) = lose m


elimOneStrategy :: Strategy
elimOneStrategy [] = Paper
elimOneStrategy (m:ms)
    | m == Rock  = Scissors
    | m == Paper = Rock
    | otherwise  = Paper


leastFrequentStrategy :: Strategy
leastFrequentStrategy moves = elimOneStrategy [leastFreqMove]
        where (rc,pc,sc) = frequencies moves (0,0,0)
              leastFreqCount = minimum [rc, pc, sc]
              [rBool, pBool, sBool] = fmap (== leastFreqCount) [rc, pc, sc]
              leastFreqMove | rBool = Rock
                            | pBool = Paper
                            | sBool = Scissors

mostFrequentStrategy :: Strategy
mostFrequentStrategy moves = elimOneStrategy [mostFreqMove]
        where (rc,pc,sc) = frequencies moves (0,0,0)
              mostFreqCount = maximum [rc, pc, sc]
              [rBool, pBool, sBool] = fmap (== mostFreqCount) [rc, pc, sc]
              mostFreqMove | rBool = Rock
                           | pBool = Paper
                           | sBool = Scissors

-----------------------------------------------------------------------------------------


t1, t2, t3, t4, t5 :: Tournament
t1 = ([Rock, Rock,Paper],[Scissors,Paper,Rock])
t2 = ([Scissors, Rock,Paper],[Scissors,Rock,Paper])
t3 = ([Paper, Rock, Paper],[Scissors, Paper, Rock])
t4 = ([Scissors, Paper, Rock],[Rock, Scissors, Paper])
t5 = ([Rock, Scissors, Paper], [Scissors, Paper, Rock])
tLong = (fst t1 ++ fst t2 ++ fst t3, snd t1 ++ snd t2 ++ snd t3 )




-- exercise 20 -------------------------------------------------------------------------


-- play strategy vs strategy
playSvsS :: Strategy -> Strategy -> Integer -> Tournament
playSvsS strat1 strat2 numRounds = applyStep strat1 strat2 numRounds ([],[])

applyStep :: Strategy -> Strategy -> Integer -> Tournament -> Tournament
applyStep _ _ 0 tournamentAccumulator = tournamentAccumulator
applyStep strat1 strat2 numRounds tAcc
    = applyStep strat1 strat2 (numRounds - 1) (step strat1 strat2 tAcc)

step :: Strategy -> Strategy -> Tournament -> Tournament
step strategyA strategyB (movesA, movesB)
    = (strategyA movesB : movesA, strategyB movesA : movesB)


{-
NOTE explanation

Mine:    | Yours:
-------------------
Rock     | Rock
Rock     | Paper
Paper    | Paper
Paper    | Scissors
Scissors | Scissors
Scissors | Rock
Rock     | Rock
Rock     | Paper
Paper    | Paper
Paper    | Scissors


if strat1 = echo and strat2 = beatLastMove then
(the reaction step results are angled across from the initializer)

1. echo [] -> Rock
2. beatLast [] is Rock
3. echo Rock is Rock
4. beatLast Rock is Paper (cross top left Rock to reach bottom right square Paper)
5. echo Paper (from result of step 4) is Paper
6. beatLast Paper is Scissors (cross top left Paper to result in bottom right Scissors)
...
-}







-- exercise 21 ------------------------------------------------------------------------

showTournament :: Tournament -> String
showTournament t@(myMoves, yourMoves) = topLine ++ topDashes ++
    concat [(justifyAndLine m) ++ show y ++ "\n" | (m,y) <- zip myMoves yourMoves]
    ++ showScore t
    where topLine = "\nStrategy A: | Strategy B:\n"
          topDashes = take (length topLine - 2) (repeat '-') ++ "\n"
          justifyAndLine m = take (lenM m) (justify m) ++ "| "
          lenM m = length (justify m)
          justify m = unpack (justifyLeft n ' ' (pack (show m)))
          n = length space
          space = take lenUntilTopBar (repeat ' ')
          lenUntilTopBar = length ("Strategy A: ")

-- note assume that even though responses are crosswise, we do scores horizontally,
-- which is pairwise
-- returns outcome: sum of scores, and individual scores (sum of +1 and -1 and 0)
showScore :: Tournament -> String
showScore (aMoves, bMoves) = total ++ scores ++ verdict ++ "\n"
    where points = [outcome a b | (a,b) <- zip aMoves bMoves ]
          numOnes = length [1 | p <- points, p == 1]
          numZeroes = length [1 | p <- points, p == 0]
          numNegOnes = length [1 | p <- points, p == (-1)]
          totalNum = sum points
          total =  "\nTotal:            " ++ (show (totalNum)) ++ "\n"
          scores = "\nScore strategy A: " ++ (show numOnes) ++ "\n" ++
                   "Score strategy B: " ++ (show numNegOnes) ++ "\n" ++
                   "Ties:             " ++ (show numZeroes) ++ "\n\n"
          verdict | totalNum == 0 = "Win: Tie"
                  | totalNum < 0 = "Win: Strategy B"
                  | otherwise  = "Win: Strategy A"


-- note assume most recent is first so show that last in the tournament box.
printTournament :: Tournament -> IO()
printTournament (myMoves, yourMoves) = putStrLn $ showTournament (reverse myMoves,
                                                                  reverse yourMoves)