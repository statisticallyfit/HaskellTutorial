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






--- SIMULATION ---------------------------------------------------------------------------

