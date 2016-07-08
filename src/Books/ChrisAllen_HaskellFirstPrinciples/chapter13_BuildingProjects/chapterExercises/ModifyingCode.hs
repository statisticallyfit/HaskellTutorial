import Control.Monad


palindrome :: IO()
palindrome = forever $ do
    line <- getLine
    case (line == reverse line) of
        True -> putStrLn "YES"
        False -> putStrLn "NO"