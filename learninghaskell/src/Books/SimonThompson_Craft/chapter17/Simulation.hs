import Data.List
import Numeric
import Test.QuickCheck


--- Queue ---------------------------------------------------------------------------------


{-
-- EXAMPLE  get input of customer arrivals (queues) and give output their
departures (decide how many bank clerks need to be working at certain times of the day).

1. type of input message: Inmess.
At a given time
    => no one arrives (No)
    => someone arrives (Yes (arrival time of customer) (time to serve them))

    => data InputMsg = No | Yes Arrival Service
       type Arrival = Integer
       type Service = Integer

2.outmess - type of output messages.At a given time, either
    =>  no one leaves (None)
    => or a person is discharged (Discharge) which takes the time the customer
    waited and time of their arrival and time it took to serve them.

    data OutputMsg = None | Discharge Arrival Wait Service
    type Wait = Integer

3. Two operations in a queue: add item and process quque by one-minute step.
This gives one minute's further processing to item at head of queue.
Two outcomes are possible: item might have its processing completed, generates an Outmess,
or further processing is needed.

4. ServerState models serving more than one quque. System receives one Inmess per minute.
Three principal operations on a server (collection of queues):
    1. add inmess to one of the queues.
    2. processing step
    3. combine a server step with allocation of Inmess to shortest queue in the server.

Three other operations are necessary:
    1. make a starting server with appropriate number of empty queues.
    2. identify num queues in server
    3. identify shrotest queue in server.
-}


-- note the "time" variables here are times (amounts) not actual clock point in time
-- such as 5:45 pm.
type Arrival = Integer -- the minute at which the customer arrives.
type Service = Integer -- time it WILL take to service a customer.
type Wait = Integer -- amount of time it took for customer to wait to get their service.
type Time = Integer -- the current time (incremented throughout all operations like clock)

-- Example: No = no arrival.
-- Example: Yes 34 12 = customer arrived at 34th minute + will need 12 min to be served.
data Inmess = No | Yes Arrival Service deriving (Eq, Show)

-- Example: None = no person has been discharged
-- Example: Discharge 34 27 12 = person arriving at 34th minute waited 27 minutes
-- to get their 12 minutes of service.
data Outmess = None | Discharge Arrival Wait Service deriving (Eq, Show)

-- [Inmess] = the actual queue.
data QueueState = QS Time Service [Inmess] deriving (Eq, Show)

addMessage :: Inmess -> QueueState -> QueueState
addMessage im (QS time serv q) = QS time serv (q ++ [im])

-- note If no inmess queue to process, incr time by one  and return empty in/outmesses.
--- > If serving time so far is less than required (servTime) then processing is
-- not complete. Therefore add one to current time and to service time so far.
--- > Otherwise the new state of the queue has time advanced by one, head removed,
-- processing time is set to 0. Output message is produced:
-- waiting time = current time subtracted by both service and arrival times.
--- > We incrememnt currTime in both cases because it took 1 minute to conduct this
-- queueStep operation.
-- note case of messages "No" are filtered out by server.
queueStep :: QueueState -> (QueueState, [Outmess])
queueStep (QS currTime servTimeSoFar []) = (QS (currTime+1) servTimeSoFar [], [])
queueStep (QS currTime servTimeSoFar inQ@(Yes arrTime servTimeReq : inRest))
    | servTimeSoFar < servTimeReq -- if serve time so far < serve time required ...
        = (QS (currTime+1) (servTimeSoFar+1) inQ, [])
    | otherwise
        = (QS (currTime+1) 0 inRest,
                [Discharge arrTime waitTime servTimeReq]) ------ this is the outmess
    where waitTime = currTime - servTimeReq - arrTime

queueStart :: QueueState
queueStart = QS 0 0 []

queueLength :: QueueState -> Int
queueLength (QS _ _ q) = length q

queueEmpty :: QueueState -> Bool
queueEmpty (QS _ _ q) = q == []






--- Server ------------------------------------------------------------------------------

newtype ServerState = SS [QueueState] deriving (Eq, Show)

numQueues :: Int
numQueues = 3

-- precondition: n < length st. If bigger, we get error from (!!) index too large.
addToQueue :: Int -> Inmess -> ServerState -> ServerState
addToQueue n im (SS st)
    = SS (take n st ++ [newQueueState] ++ drop (n+1) st)
      where
      newQueueState = addMessage im (st !! n) -- add to the nth queue counting from 0.

-- note step of server means making a step in each of the queues server holds and
-- concatenating together the output messages they produce.
serverStep :: ServerState -> (ServerState, [Outmess])
serverStep (SS []) = (SS [], [])
serverStep (SS (q:qs)) = (SS (q':qs'), outMess ++ outMesses)
    where
    (q', outMess) = queueStep q
    (SS qs', outMesses) = serverStep (SS qs)

-- note do server step then add incoming message. If message indicates arrival, then
-- add it to shortest queue.
simulationStep :: ServerState -> Inmess -> (ServerState, [Outmess])
simulationStep ss im = (addNewObject im ss', outMess)
    where (ss', outMess) = serverStep ss

-- note adds message to shortest queue
-- note it is here that inmess with type (No) are not passed to the queues.
addNewObject :: Inmess -> ServerState -> ServerState
addNewObject No servSt = servSt
addNewObject inmess@(Yes arr wait) servSt
        = addToQueue (indexOfShortestQueue servSt) inmess servSt

-- note numQueues is a constant to be defined.
serverStart :: ServerState
serverStart = SS (replicate numQueues queueStart)

-- note returns num of queues.
serverSize :: ServerState -> Int
serverSize (SS xs) = length xs

-- note returns index of shortest queue.
indexOfShortestQueue :: ServerState -> Int
indexOfShortestQueue (SS [q]) = 0
indexOfShortestQueue (SS (q:qs))
    | (queueLength (qs !! short)) <= queueLength q = short + 1
    | otherwise = 0
    where short = indexOfShortestQueue (SS qs)




ins1, ins2, ins3 :: [Inmess]
ins1 = [Yes 46 3, Yes 55 10, Yes 65 2, Yes 80 15, Yes 95 1, Yes 96 2]
ins2 = [Yes 10 4, Yes 14 4, Yes 18 9, Yes 27 3, Yes 30 4]
ins3 = [Yes 1 17, Yes 18 5, Yes 23 2, Yes 25 4, Yes 29 1, Yes 30 8]

qstate1, qstate2, qstate3 :: QueueState
qstate1 = QS 0 0 ins1
qstate2 = QS 0 0 ins2
qstate3 = QS 0 0 ins3

ss1 :: ServerState
ss1 = SS [qstate1, qstate2, qstate3]





--- SIMULATION ---------------------------------------------------------------------------

seed = 17489
multiplier = 25173
increment = 13849
modulus = 65536
dist = [(1, 0.2), (2, 0.25), (3, 0.25), (4, 0.15), (5, 0.1), (6, 0.05)]

-- waiting times range from 1 to 6 minutes but they happen with different probabilities:
-- note:
{-
WAIT TIME:   |  1  |  2   |  3   |   4  |  5   |  6
------------------------------------------------------
PROBABILITY: | 0.2 | 0.25 | 0.25 | 0.15 | 0.10 | 0.05
-}


nextRand :: Integer -> Integer
nextRand n = (multiplier * n + increment) `mod` modulus

randomSequence :: Integer -> [Integer]
randomSequence sd = iterate nextRand sd -- the sd == seed

-- makes random numbers be in range from <= x <= to
-- precondition: from > to otherwise numbers won't be completely in that range.
scaleSequence :: Integer -> Integer -> [Integer] -> Maybe [Integer]
scaleSequence from to sequence
    | from > to = Just $ map scale sequence
    | otherwise = Nothing
    where
    scale n = n `div` denom + from
    range = to - from +1
    denom = modulus `div` range

-- note transforms a distribution (discrete) into a transformer of infinite lists.
makeFunction :: [(a, Float)] -> Float -> a
makeFunction dist = makeFun dist 0.0 {-takes rand arg here below -}

-- note b == float type
-- note once prob * modulus + nLast is greater than rand then return time.
 -- help todo understand better.
makeFun :: (Num b, Ord b) => [(a, b)] -> b -> b -> a
makeFun ((time, prob) : distRest) nLast rand
    | nNext >= rand && rand > nLast = time
    | otherwise = makeFun distRest nNext rand
    where nNext = prob * fromIntegral modulus + nLast


-- NOTE: generates the waiting times based on the probability distribution given
-- and the random sequence of numbers.
randomTimes :: [Integer]
randomTimes = map (makeFunction dist . fromIntegral) (randomSequence seed)


-- note creates the (in:messes) list using random times
-- note key Yes is a function constructor that is applied to infinite list [1..] to
-- zip it with random times.
simulationInput :: [Inmess]
simulationInput = zipWith Yes [1..] randomTimes

simulationInputNo :: [Inmess]
simulationInputNo = take 50 simulationInput ++ noes
    where noes = No : noes

-- note run like this: doSimulation serverStart simulationInput
-- note: key: returns the combined outmesses of the function:
-- simulationStep serverStart anInmess, (note serverStart becomes next).
doSimulation :: ServerState -> [Inmess] -> [Outmess]
doSimulation servSt (im:messes)
    = outmesses ++ doSimulation servStNext messes
    where
    (servStNext, outmesses) = simulationStep servSt im




totalWait :: [Outmess] -> Integer
totalWait {-outmesses-} = sum . map waitTime -- outmesses
    where waitTime (Discharge _ w _) = w




















------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------




--- TESTING QueueState ------------------------------------------------------------------

instance Arbitrary Inmess where
    arbitrary = do
        Positive arr <- arbitrary
        Positive serv <- arbitrary
        oneof [return No, return (Yes arr serv)]

instance Arbitrary Outmess where
    arbitrary = do
        Positive arr <- arbitrary
        Positive wait <- arbitrary
        Positive serv <- arbitrary
        oneof [return None, return (Discharge arr wait serv)]

instance Arbitrary QueueState where
    arbitrary = do
        Positive time <- arbitrary
        Positive service <- arbitrary
        inmesses <- arbitrary
        return (QS time service inmesses)

------------------------------------------------------------------------------------------
-- note too costly to use Property to limit all inmesses to have all yes values and
-- no Nos. Must declare the arbitrary instance to have thsi property ingrained in it from
-- the start, instead.

-- note check that im is added last in addMessage
propQ_AddMessage :: Inmess -> QueueState -> Bool
propQ_AddMessage im q = last inmesses == im
    where q'@(QS time serv inmesses) = addMessage im q

-- note check that discharge is only present in outmess when service time is 0,
-- and when servtime is not zero, there are no discharges.
propQ_QueueStep :: QueueState -> Bool
propQ_QueueStep q@(QS ct st ins) = servIsZeroANDOuts || servNotZeroANDNoOuts
    where filteredIns = filter (/= No) ins
          (QS ct' st' ins', outs) = queueStep (QS ct st filteredIns)
          servIsZeroANDOuts = st' == 0 && (not $ null outs)
          servNotZeroANDNoOuts = st' /= 0 && (null outs)








--- TESTING SERVER ------------------------------------------------------------------------

instance Arbitrary ServerState where
    arbitrary = do
        qs <- arbitrary
        return (SS qs)

------------------------------------------------------------------------------------------

-- note check that AddToQueue function really adds to the nth queue (counting from 0)
-- note how to: the nth queue in serverstate should have last element equal to (im).
propS_Add :: Int -> Inmess -> ServerState -> Property
propS_Add n im (SS qs) =
    n >= 0 && n < (length qs) ==>
    (last inmesses) == im
    where (SS qs') = addToQueue n im (SS qs)
          (QS _ _ inmesses) = qs' !! n

-- note num of queues should be same as before
propS_Add2 :: Int -> Inmess -> ServerState -> Bool
propS_Add2 n im (SS qs) = length qs' == length qs
    where (SS qs') = addToQueue n im (SS qs)


-- note check that all queues satisfy prop queues once serverStep has been done.
{-
HELP can't get it to work because outs are clobbered together and cannot check
serve == 0 and outs condition individually, since don't know where the outs were
put together

propS_ServerStep :: ServerState -> Bool
propS_ServerStep (SS qs) = allServZeroANDOuts || allServNotZeroANDNoOuts
    where deleteNos ins = filter (/= No) ins
          qsAllYes = map (\(QS t s ins) -> (QS t s (deleteNos ins))) qs
          result@(SS qs', outs) = serverStep (SS qsAllYes)
          allServZeroANDOuts = and $ map (\(QS _ serv _, outs)
                                        -> serv == 0 && (not $ null outs)) result
          allServNotZeroANDNoOuts = and $ map (\(QS _ serv _, outs)
                                        -> serv /= 0 && (null outs)) result
-}







--- TESTING Simulation ------------------------------------------------------------------

--- testing scaleSequence: min is from and max is to.
testScaleSequence :: Integer -> Integer -> Bool
testScaleSequence from to = areNothing || fromToAreMinMax
    where scaledMaybe = scaleSequence from to (randomSequence seed)
          (Just scaled) = scaledMaybe
          areNothing = if from <= to then True else False
          fromToAreMinMax = (minimum scaled == from) && (maximum scaled == to)

--- todo: testing makeFunction
--- todo: testing queueStep
--- todo: testing serverStep


--- todo: here below was testing if distribution of waiting times created using
-- randomTimes was poisson (since dist seems poisson, but this one below seems uniform)
-- Why? help todo
occ xs n = length $ elemIndices n xs
rts = take 10000 randomTimes

freqs = (map $ occ rts) [1,2,3,4,5,6]

prob x = fromIntegral x / (fromIntegral $ sum rts)

probabilities = map prob freqs

showDecimal x = showFFloat Nothing x ""

showProbs = map showDecimal probabilities