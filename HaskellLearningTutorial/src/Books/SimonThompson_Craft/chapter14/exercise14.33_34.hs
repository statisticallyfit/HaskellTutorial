
type Name = String
type FuelEfficiency = Int

data Car = Car Name FuelEfficiency deriving (Eq, Show)

data ParkingLot = Lot [Car]  deriving (Eq, Show)


car1, car2, car3 :: Car
car1 = Car "Honda" 23
car2 = Car "Peugeot" 50
car3 = Car "Volkswagen" 150

lot1 :: ParkingLot
lot1 = Lot [car1, car2, car3, car2]


main = do
    print $ lot1
    print car1
    print car2
    print car3