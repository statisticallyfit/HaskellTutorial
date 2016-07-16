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

-- help help help todo: why doesn't it work to give argument right in here?
-- help where is the Monad? How is tuple a monad... ?
tupledD :: [Char] -> ([Char], [Char])
tupledD = do
    a <- rev
    b <- cap
    return (a, b)


-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
tupledB :: [Char] -> ([Char], [Char])
tupledB = rev >>=
            \revStr ->
                cap >>=
                    \capStr -> return (revStr, capStr)
-- help help why doesn't this work:
{-tupledB s = rev s >>=
            \revStr ->
                cap s >>=
                    \capStr -> return (revStr, capStr)
-}



main = do
    print $ fmapped "Julie" == "EILUJ"
    print $ composed "Chris" == "SIRHC"
    print $ tupledA "Julie" == ("eiluJ","JULIE")
    print $ tupledB "Julie" == ("eiluJ","JULIE")
    print $ tupledD "Julie" == ("eiluJ","JULIE")
