type Name    = String
type Price   = Int
type BarCode = Int
type Line = (Name, Price)

type Database = [(BarCode, Name, Price)]
type TillType = [BarCode]
type BillType = [Line]


-- bill line length
lineLength :: Int
lineLength = 30

formatPence :: Price -> String
formatPence p = show pounds ++ "." ++ (pad pence)
                where pounds = p `div` 100
                      pence = p `mod` 100
                      pad rest = if rest < 10
                                 then "0" ++ (show rest)
                                 else show rest

formatLine :: Line -> String
formatLine (n,p) = n ++ (replicate diff '.') ++ p' ++ "\n"
                   where diff = lineLength - (length (n ++ p'))
                         p' = formatPence p

formatLines :: [Line] -> String -- BillType -> String
formatLines billList = "\n" ++ [line | npTuple <- billList, line <- formatLine npTuple]

testFormatLines :: [Line]
testFormatLines = [ ("Fish Fingers", 121),
                    ("Nappies", 1010),
                    ("Orange Jelly", 56),
                    ("Hula Hoops", 21),
                    ("Hula Hoops (Giant)", 133),
                    ("Dry Sherry, 1lt", 540)]



-- exercise 17 --------------------------------------------------------------------
formatList :: (a -> String) -> [a] -> String
formatList f list = foldr (++) [] (map f list)

formatLines' :: [Line] -> String
formatLines' billList = formatList formatLine billList


printLines :: [Line] -> IO()
printLines billList = putStrLn $ formatLines billList

printLines' :: [Line] -> IO()
printLines' billList = putStrLn $ "\n" ++ formatLines' billList