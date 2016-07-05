

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgrammingLanguage = Haskell | Agda | Idris | PureScript deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem, lang :: ProgrammingLanguage}
    deriving (Eq, Show)

allOS :: [OperatingSystem]
allOS = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer o l | o <- allOS, l <- allLanguages]


testLen = length allProgrammers == (length allOS * length allLanguages)