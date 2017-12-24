

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

type Size = Integer
data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)


myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir 123823


--- 1
{-
*Main> :t myCar
myCar :: Vehicle
*Main> :t urCar
urCar :: Vehicle
*Main> :t clownCar
clownCar :: Vehicle
*Main> :t doge
doge :: Vehicle
-}


--- 2
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars {-vs-} = map isCar {-vs-}



--- 3
getManu :: Vehicle -> Maybe Manufacturer
getManu (Car m _) = Just m
getManu _ = Nothing