type Name = String
type Address = String

data Person = Adult Name Address Bio
            | Child Name
            deriving (Eq, Show)

data Bio    = Parent String [Person]
            | NonParent String
            deriving (Eq, Show)


percyPerson, percyMother, percyFather, child :: Person
percyPerson = Adult "\nPercy Jackson" "3541 Northside, New York" percyBio
percyFather = Adult "\nPoseidon" "Undersea" (NonParent "")
percyMother = Adult "\nSally Jackson" "3541 Northside, New York" percyMotherBio
child = Child "\nIsabel Gabriela-Huerta"

percyBio, percyMotherBio :: Bio
percyBio = Parent "\nPercy's parents: " [percyFather, percyMother]
percyMotherBio = Parent "\nSally's parent: " [Adult "\nMargaret Doe" "\nForest"
                        (Parent "" [])]


showPerson :: Person -> String
showPerson (Child n) = show n
showPerson (Adult n a b) = show n ++ show a ++ showBio b

showBio :: Bio -> String
showBio (Parent st ps) = st ++ concat (map showPerson ps)
showBio (NonParent st) = st