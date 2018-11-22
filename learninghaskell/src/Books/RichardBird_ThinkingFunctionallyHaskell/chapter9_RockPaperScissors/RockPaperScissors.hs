module RockPaperScissors where


import System.Random


data Move = Rock | Paper | Scissors deriving (Eq, Show)
type Round = (Move, Move)
-- Strategy 1: function that takes finite list of moves made by opponent so far and returns
-- appropriatemove for (other player?) for next round. Assume list is given in reverse order with
-- most recent move first.
type Strategy1 = [Move] -> Move
-- Strategy 2: function that takes potentially infinite list of moves made by opponent and
-- returns potentially infinite list of replies.
type Strategy2 = [Move] -> [Move]




-- to score a round
score :: Round -> (Int, Int)
score (x,y) | x `beats` y = (1, 0)
            | y `beats` x = (0, 1)
            | otherwise = (0, 0)

beats :: Move -> Move -> Bool
Paper `beats` Rock = True
Rock `beats` Scissors = True
Scissors `beats` Paper = True
_ `beats` _ = False



-- For strategy 1: ------------------------------------------------------------------------------

-- note determines total score after playing given number of rounds
match1 :: Int -> (Strategy1, Strategy1) -> (Int, Int)
match1 n = total . map score . take n . rounds1
    where total rs = (sum (map fst rs), sum (map snd rs))


-- note every player is a strategy.
-- Strategy 1: Copy
-- note given list of moves with recent first, return arbitrary choice rock if null otherwise
-- copy the player's move.
copy1 :: Strategy1
copy1 ms = if null ms then Rock else head ms


-- Stratey 2: smart: study num times opponent has produeced each of three moves
-- and calculate appropriate response based on probabilities
-- note foldr (\x acc) and foldl (\acc y)
smart1 :: Strategy1
smart1 ms = if null ms then Rock else pick (foldr count (0, 0, 0) ms)


--  foldr list elem    foldr acc
count :: Move -> (Int, Int, Int) -> (Int, Int, Int)
count Paper (p, r, s) = (p+1, r, s)
count Rock (p, r, s) = (p, r+1, s)
count Scissors (p, r, s) = (p, r, s+1)


{-note: m can either be:
1 ==>     0 <=  m  <  p
2 ==>     p <=  m  <  p + r
3 ==> p + r <=  m  <  p + r + s
Explanation:
if p is large then Scissors is chosen with high probability (scissors beats paper)
if r is large, then Paper is chosen (paper beats rock)
if s is large then Rock is chosen (scissors beats rock)
note: mkStdGen takes int and returns rand num generator. Its arg n is arbitrary.
note: randomR takes range (a, b) and generator and returns rand int in
the range a <= r <= b and new rand num generator.
-}
pick :: (Int, Int, Int) -> Move
pick (p, r, s)
    | m < p         = Scissors
    | m < p + r     = Paper
    | m < p + r + s = Rock
    where m = rand (p + r + s)

-- note for given n, always yields same rand num since no state
rand n = fst $ randomR (0, n-1) (mkStdGen n)


-- compare with rounds2 (rounds1 is inefficient due to Strategy1 structure)
-- note makes infinite list of rounds that ensue when each player (each strategy) acts.
-- Repeatedly calls extends.
    -- note instead of saying map head $ tail $ iterated result you can just take last list
    -- in the iterated result.
rounds1 :: (Strategy1, Strategy1) -> [Round]
rounds1 (s1, s2) = map head $ tail $ iterate (extend (s1, s2)) []

-- note extend adds new pair of moves to front of list using cyclic method.
extend :: (Strategy1, Strategy1) -> [Round] -> [Round]
extend (s1, s2) rs = (s1 (map snd rs), s2 (map fst rs)) : rs






-- For Strategy2 ------------------------------------------------------------------------------


-- type Strategy2 = [Move] -> [Move]

copy2 :: Strategy2
copy2 ms = Rock : ms

-- note stats computes running counts of three possible moves.
smart2 :: Strategy2
smart2 ms = Rock : map pick (stats ms)
    where stats = tail . scanl (flip count) (0,0,0) {-ms arg goes here-}

-- note rounds2 is defined by two cyclic lists.
rounds2 :: (Strategy2, Strategy2) -> [Round]
rounds2 (p1, p2) = zip xs ys
    where xs = p1 ys
          ys = p2 xs

-- note: rounds2 offers no protection against someone who cheats!
cheat :: [Move] -> [Move]
cheat ms = map trump ms
    where trump Paper = Scissors
          trump Rock = Paper
          trump Scissors = Rock

-- behaves like copy for n moves then starts to cheat
devious :: Int -> Strategy2
devious n ms = take n (copy2 ms) ++ cheat (drop n ms)



-- version of rounds2 that prevents cheating
rounds2' :: (Strategy2, Strategy2) -> [Round]
rounds2' (p1, p2) = zip xs ys
    where xs = police p1 ys
          ys = police p2 xs


police :: Strategy2 -> [Move] -> [Move]
police p ms = ms'
    where ms' = p (synch ms ms')

-- note what is the correct definition for synch? 
synch :: [Move] -> [Move] -> [Move]
{-synch [] [] = []
synch [x] [y] = [y]
synch [] [y] = [y]
synch [x] [] = []-}
synch (x:xs) (y:ys) = (y `seq` x) : (synch xs ys)


-- let ms = [Rock, Paper, Scissors , Rock , Paper , Scissors , Rock , Rock, Rock, Rock, Rock]

---------------------------------------
onSeperateLines :: [Round] -> IO()
onSeperateLines rs = putStrLn $ concatMap (++ "\n") $ map show rs
