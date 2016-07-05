

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving (Eq, Show)

type Gardener = String

data Garden = Garden Gardener FlowerType
    deriving (Eq, Show)


data GardenNormal = Gardenia' Gardener
                  | Rose' Gardener
                  | Daisy' Gardener
                  | Lilac' Gardener
                  deriving (Eq, Show)