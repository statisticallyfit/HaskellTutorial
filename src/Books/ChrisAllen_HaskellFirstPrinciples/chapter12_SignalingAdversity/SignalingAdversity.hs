

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving (Eq, Show)

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (checkName name) (checkAge age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right n) (Right a) = Right (Person n a)
mkPerson' (Left n) (Left a) = Left (n ++ a)
mkPerson' (Left n) _ = Left n
mkPerson' _ (Left a) = Left a


checkAge :: Age -> Either [PersonInvalid] Age
checkAge age = case age >= 0 of
                  True -> Right age
                  False -> Left [AgeTooLow]

checkName :: Name -> Either [PersonInvalid] Name
checkName name = case name /= "" of
                    True -> Right name
                    False -> Left [NameEmpty]

personOK = mkPerson "Catherine" 15
personERAge = mkPerson "Beatrice" (-1)
personERName = mkPerson "" 90
personERBoth = mkPerson "" (-3)
