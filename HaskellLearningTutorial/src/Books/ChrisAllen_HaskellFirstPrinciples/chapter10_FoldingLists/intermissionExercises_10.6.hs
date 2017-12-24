import Data.Time


data DatabaseItem = DbString String
                  | DbNumber Int
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDataBase :: [DatabaseItem]
theDataBase = [DbDate (UTCTime
                            (fromGregorian 1911 5 1)
                            (secondsToDiffTime 34123)),
               DbNumber 9001,
               DbString "Hello, world!",
               DbNumber 187234,
               DbNumber 17,
               DbDate (UTCTime
                            (fromGregorian 1921 5 1)
                            (secondsToDiffTime 34123))]

--- 1

-- note the otherConstructor will go again into the accumulation and will be evaluated
-- by function (f).
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate dBase = foldr f [] dBase
    where f (DbDate utc) otherConst = utc : otherConst
          f _ otherConst = otherConst

--- 2
filterDbNumber :: [DatabaseItem] -> [Int]
filterDbNumber dBase = foldr f [] dBase
    where f (DbNumber n) otherConst = n : otherConst
          f _ otherConst = otherConst

--- 3
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent dBase = maximum $ filterDbDate dBase

-- note the function (f) takes constructor with utc on left and just a utc on the right
-- because we are using foldr. So there is the first argument (DbDate time1) from the
-- data base `f` with the seed longTimeAgo.
-- note the function f only accepts DbDate constructor on the left because next
-- case it has _ (means any other constructor kind or argument).
mostRecent' :: [DatabaseItem] -> UTCTime
mostRecent' dBase = foldr f longTimeAgo dBase
    where longTimeAgo = UTCTime (fromGregorian 0 1 1) (secondsToDiffTime 0)
          f (DbDate time1) time2
            | time1 > time2 = time1
            | otherwise = time2
          f _ time = time

-- note if encounter constructor other than DbNumber, return just prevNum. Else, compare
-- the two nums and decide which is bigger. Fold this operation over entire list.
highestNum :: [DatabaseItem] -> Int
highestNum dBase = foldr f lowestInt dBase
    where lowestInt = minBound :: Int
          f (DbNumber nextNum) prevNum
            | nextNum > prevNum = nextNum
            | otherwise = prevNum
          f _ num = num



--- 4
sumDb :: [DatabaseItem] -> Int
sumDb dBase = foldr f 0 dBase
    where f (DbNumber nextNum) prevNum = nextNum + prevNum
          f _ num = num


--- 5
avgDb :: [DatabaseItem] -> Double
avgDb dBase = average $ filterDbNumber dBase
    where average [] = 0
          average xs = fromIntegral (sum xs) / fromIntegral (length xs)