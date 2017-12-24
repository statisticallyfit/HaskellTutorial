
type Name    = String
type Price   = Int
type BarCode = Int

type Database = [(BarCode, Name, Price)]
type TillType = [BarCode]
type BillType = [(Name, Price)]



-- bill line length
lineLength :: Int
lineLength = 30



testFormatBill :: BillType
testFormatBill = [ ("Dry Sherry, 1lt", 540),
                   ("Fish Fingers", 121),
                   ("Orange Jelly", 56),
                   ("Hula Hoops (Giant)", 133),
                   ("Dry Sherry, 1lt", 540) ]

testFormatBill2 :: BillType
testFormatBill2 = testFormatBill ++ [("Dry Sherry, 1lt", 540),
                                     ("Dry Sherry, 1lt", 540),
                                     ("Dry Sherry, 1lt", 540),
                                     ("Dry Sherry, 1lt", 540),
                                     ("Dry Sherry, 1lt", 540),
                                     ("Dry Sherry, 1lt", 540),
                                     ("Dry Sherry, 1lt", 540)]

------------------------------------------------------------------------------------

formatPence :: Price -> String
formatPence p = show pounds ++ "." ++ (pad pence)
                where pounds = p `div` 100
                      pence = p `mod` 100
                      pad rest = if rest < 10
                                 then "0" ++ (show rest)
                                 else show rest


formatLine :: (Name, Price) -> String
formatLine (n,p) = n ++ (replicate diff '.') ++ p' ++ "\n"
                   where diff = lineLength - (length (n ++ p'))
                         p' = formatPence p

formatLines :: [(Name, Price)] -> String -- BillType -> String
formatLines billList = concat $ map formatLine billList

formatTotal :: Price -> String
formatTotal p = "\nTotal" ++ replicate diff '.' ++ p'
                where diff = lineLength - length (p' ++ "Total")
                      p' = formatPence p


makeDiscount :: BillType -> Price
makeDiscount billList = numBottles `div` 2
       where numBottles = sum $ map getSherries billList
             getSherries (n,p) = if n == "Dry Sherry, 1lt" then 1 else 0

makeTotal :: BillType -> Price
makeTotal billList = sum $ map getPrice billList
    where getPrice (b,p) = p

formatDiscount :: Price -> String
formatDiscount p = "\nDiscount" ++ replicate diff '.' ++ p' ++ "\n"
                   where diff = lineLength - length (p' ++ "Discount")
                         p' = show p ++ ".00"

formatBill' :: BillType -> String
formatBill' billList = formatLines billList
                    ++ formatDiscount (makeDiscount billList)
                    ++ formatTotal (makeTotal billList)





printBill :: BillType  -> IO()
printBill {-billList-} = putStrLn . formatBill' {-billList-}

printBill' :: BillType  -> IO()
printBill' = formatBill' >.> putStrLn


-- note define
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f . g