import Data.Char


cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs


composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap



tupledA :: [Char] -> ([Char], [Char])
tupledA str = ((,) <$> rev <*> cap) str


tupledM :: [Char] -> ([Char], [Char])
tupledM str =


main = do
    print $ fmapped "Julie" == "EILUJ"
    print $ composed "Chris" == "SIRHC"
    print $ tupledA "Julie" == ("eiluJ","JULIE")
