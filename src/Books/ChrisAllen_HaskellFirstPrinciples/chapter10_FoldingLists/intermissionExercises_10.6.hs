import Data.Time


data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDataBase :: [DatabaseItem]
theDataBase = [DbDate (UTCTime
                            (fromGregorian 1911 5 1)
                            (secondsToDiffTime 34123)),
               DbNumber 9001,
               DbString "Hello, world!",
               DbNumber 187234,
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
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber dBase = foldr f [] dBase
    where f (DbNumber n) otherConst = n : otherConst
          f _ otherConst = otherConst

--- 3
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent dBase = maximum $ filterDbDate dBase

-- note the function (f) takes constructor with utc on left and just a utc on the right
-- because we are using foldr. So first comparison
mostRecent' :: [DatabaseItem] -> UTCTime
mostRecent' dBase = foldr f longTimeAgo dBase
    where longTimeAgo = UTCTime (fromGregorian 0 1 1) (secondsToDiffTime 0)
          f (DbDate time1) time2
            | time1 > time2 = time1
            | otherwise = time2
          f _ time = time