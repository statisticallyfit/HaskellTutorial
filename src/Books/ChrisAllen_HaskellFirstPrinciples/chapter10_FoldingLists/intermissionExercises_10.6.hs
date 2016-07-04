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