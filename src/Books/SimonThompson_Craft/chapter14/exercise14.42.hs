
data Season = Spring | Summer | Autumn | Winter deriving (Eq, Ord, Show)

data Temp = Hot | Cold deriving (Eq, Ord, Show)


newWeather :: Season -> Temp
newWeather = makeHot . isSummer

makeHot :: Bool -> Temp
makeHot True = Hot
makeHot False = Cold

isSummer :: Season -> Bool
isSummer = (== Summer)


main = do
    print $ newWeather Summer
    print $ newWeather Spring
    print $ newWeather Winter
    print $ newWeather Autumn