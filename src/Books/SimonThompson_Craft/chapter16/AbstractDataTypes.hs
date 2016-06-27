import Test.QuickCheck
import Control.Monad

{-
NOTE building calculator for numerical expressions.
Sets values of variables.
Store: current variable values.

Abstract data type (ADT): limited interface by means of specified set of
operatoins.
Example: we hide the information about how type Store is implemented.
Also note: if we export a type like this => Data without (..) then constructors are
not exported so we can only operate on type through functions defined on it.
Example module Store (Store, initial, value, update) where
Here, constructors of Store are not available so we can only operate on Store using its
functions.

-}


type Var = Char -- HELP what is Var supposed to be?
newtype Store = Store [(Integer, Var)] -- deriving (Eq, Show)


initial :: Store
initial = Store []

value :: Store -> Var -> Integer
value (Store []) v = 0
value (Store ((n,w) : rest)) v
    | v == w  = n
    | otherwise = value (Store rest) v

-- note put the new pair at the font.
update :: Store -> Var -> Integer -> Store
update (Store sto) v n = Store ((n,v):sto)


instance Eq Store where
    (Store s1) == (Store s2) = (s1 == s2)

instance Show Store where
    showsPrec n (Store s) = showsPrec n s


sto1 = Store [(10,'a'), (2,'b'), (2,'b'), (0,'j'), (99,'k'),(101,'k')]
sto2 = Store [(10,'z'), (3,'z'), (2,'p'), (8,'m'), (7,'n'), (5,'a'), (3,'b')]
sto3 = Store [(10,'z'), (2,'m'), (2,'b'), (0,'g'), (99,'e'), (101,'a')]




------------------------------------------------------------------


newtype Stock = Stock (Var -> Integer)

initial' :: Stock
initial' = Stock (\v -> 0)

value' :: Stock -> Var -> Integer
value' (Stock s) v = s v

-- HELP understand better.
update' :: Stock -> Var -> Integer -> Stock
update' (Stock s) v n = Stock (\w -> if w == v then n else (s w))




------------------------------------------------------------------


instance Arbitrary Store where
    arbitrary = do
        x <- arbitrary --integer
        y <- arbitrary -- char (var)
        return (Store [(x,y)])



-- Testing ADTs
propInitial :: Char -> Bool
propInitial ch = value initial ch == 0

-- note once we update, when we look up value, we expect to see the new value.
propUpdate1 :: Char -> Integer -> Store -> Bool
propUpdate1 c n sto = value (update sto c n) c == n

-- note after an update, if we look up value of another variable, its value should
-- be same as before
-- HELP TODO understand better.
propUpdate2 :: Char -> Char -> Integer -> Store -> Bool
propUpdate2 c1 c2 n sto = value (update sto c2 n) c1 == value sto c1 || c1 == c2
-- note this fails when c1 == c2
--propUpdate2 c1 c2 n sto = value (update sto c2 n) c1 == value sto c1












-- 16.3 QUEUES ------------------------------------------------------------------------

data Queue a = Queue [a] deriving (Eq, Show)

empty :: Queue a
empty = Queue []

isEmpty :: Queue a -> Bool
isEmpty (Queue []) = True
isEmpty _          = False

-- note add to the end (like lineup)
add :: a -> Queue a -> Queue a
add x (Queue xs) = Queue (xs ++ [x])

-- note remove from front (like lineup getting customers)
remove :: Queue a -> (a, Queue a)
remove q@(Queue xs)
    | not (isEmpty q) = (head xs, Queue (tail xs))
    | otherwise = error "remove from front"



q1 :: Queue Integer
q1 = Queue [1,2,3,4,5,6,7,8,9,10]

q2 :: Queue String
q2 = Queue ["let's", "go", "fly", "a", "kite", "up", "to", "the", "highest", "height"]


----------------------------------------------------------------------------------------

-- note add to front
add' x (Queue xs) = Queue (x:xs)

-- note remove from last
remove' q@(Queue xs)
    | not (isEmpty q) = (last xs, Queue (init xs))
    | otherwise = error "remove' from last"



-- More efficient (but same behavior (?? - not really)) -HELP --------------------------
data QueueSplit a = QueueSplit [a] [a] deriving (Eq, Show)

emptyQS :: QueueSplit a
emptyQS = QueueSplit [] []

isEmptyQS :: QueueSplit a -> Bool
isEmptyQS (QueueSplit [] []) = True
isEmptyQS _ = False

-- note add to the head of right list
addQS :: a -> QueueSplit a -> QueueSplit a
addQS x (QueueSplit xs ys) = QueueSplit xs (x:ys)

-- note remove from head of left list
removeQS :: QueueSplit a -> (a, QueueSplit a)
removeQS (QueueSplit [] []) = error "remove QS"
removeQS (QueueSplit [] ys) = removeQS (QueueSplit (reverse ys) [])
removeQS (QueueSplit (x:xs) ys) = (x, QueueSplit xs ys)




qs1 :: QueueSplit Integer
qs1 = QueueSplit [1,2,3,4,5] [6,7,8,9,10]

qs2 :: QueueSplit String
qs2 = QueueSplit ["let's", "go", "fly", "a", "kite"]
                 ["up", "to", "the", "highest", "height"]













-- 16.5,6 SIMULATION ----------------------------------------------------------------------

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


ins1, ins2, ins3 :: [Inmess]
ins1 = [Yes 46 3, Yes 55 10, Yes 65 2, Yes 80 15, Yes 95 1, Yes 96 2]
ins2 = [Yes 10 4, Yes 14 4, Yes 18 9, Yes 27 3, Yes 30 4]
ins3 = [Yes 1 17, Yes 18 5, Yes 23 2, Yes 25 4, Yes 29 1, Yes 30 8]

qstate1, qstate2, qstate3 :: QueueState
qstate1 = QS 0 0 ins1
qstate2 = QS 0 0 ins2
qstate3 = QS 0 0 ins3



{-
EXAMPLE run:
*Main> queueStep qstate1
(QS 1 1 [Yes 46 3,Yes 55 10,Yes 65 2,Yes 80 15,Yes 95 1,Yes 96 2],[])
*Main> queueStep (fst it)
(QS 2 2 [Yes 46 3,Yes 55 10,Yes 65 2,Yes 80 15,Yes 95 1,Yes 96 2],[])
*Main> queueStep (fst it)
(QS 3 3 [Yes 46 3,Yes 55 10,Yes 65 2,Yes 80 15,Yes 95 1,Yes 96 2],[])
*Main> queueStep (fst it)
(QS 4 0 [Yes 55 10,Yes 65 2,Yes 80 15,Yes 95 1,Yes 96 2],[Discharge 46 (-46) 3])

-}



-- TESTING QueueState ---------------------------------------------------------------------

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

---------------------------------------------------
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






--- Server ------------------------------------------------------------------------------

newtype ServerState = SS [QueueState] deriving (Eq, Show)

numQueues :: Int
numQueues = 5

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
addNewObject No ss = ss
addNewObject inQ@(Yes arr wait) ss = addToQueue (shortestQueue ss) inQ ss

-- note numQueues is a constant to be defined.
serverStart :: ServerState
serverStart = SS (replicate numQueues queueStart)

-- note returns num of queues.
serverSize :: ServerState -> Int
serverSize (SS xs) = length xs

-- note returns index of shortest queue.
shortestQueue :: ServerState -> Int
shortestQueue (SS [q]) = 0
shortestQueue (SS (q:qs))
    | (queueLength (qs !! short)) <= queueLength q = short + 1
    | otherwise = 0
    where short = shortestQueue (SS qs)


{-
EXAMPLE evaluation goes downward until we evaluate all the "short". Then we go back up.

SAY lengths of (q1,q2,q3,q4) are (3,14,5,6) respectively

shortestQueue (SS [q1, q2, q3, q4]) --- > 0

------------------------------------------------------

shortestQueue (SS (q1 : [q2,q3,q4]))
= queueLen ([q2,q3,q4] !! short) <= queueLen q1
= queueLen q3 <= queueLen q1
= 5 <= 3 = False so --- > 0

=> short = shortestQueue  (SS [q2,q3,q4]) --- > 1

------------------------------------------------------
shortestQueue (SS q2 : [q3,q4])
= queueLen ([q3,q4] !! short) <= queueLen q2
= queueLen ([q3,q4] !! 0) <= queueLen q2
= queueLen q3 <= queueLen q2
= 5 <= 14 = True so --- > 0 + 1 = 1

=> short = shortestQueue (SS [q3,q4]) --- > 0

------------------------------------------------------
shortestQueue (SS q3 : [q4])
= queueLen ([q4] !! short) <= queueLen q3
= queueLen (q4) <= queueLen q4
= 6 <= 5 = False so --- > 0

=> short = shortestQueue (SS [q4]) --- > 0

-- note now evaluation goes upward.

-}

ss1 :: ServerState
ss1 = SS [qstate1, qstate2, qstate3]









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

------------------------------------------------------------------------------------------

