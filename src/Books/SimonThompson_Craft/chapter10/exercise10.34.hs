
type Name    = String
type Price   = Int
type BarCode = Int

type Database = [(BarCode, Name, Price)]
type TillType = [BarCode]
type BillType = [(Name, Price)]


-- bill line length
lineLength :: Int
lineLength = 30

codeIndex :: Database
codeIndex = [ (4719, "Fish Fingers", 121),
              (5643, "Nappies", 1010),
              (3814, "Orange Jelly", 56),
              (1111, "Hula Hoops", 21),
              (1112, "Hula Hoops (Giant)", 133),
              (1234, "Dry Sherry, 1lt", 540)]

testFormatLines :: [(Name, Price)]
testFormatLines = [ ("Fish Fingers", 121),
                    ("Nappies", 1010),
                    ("Orange Jelly", 56),
                    ("Hula Hoops", 21),
                    ("Hula Hoops (Giant)", 133),
                    ("Dry Sherry, 1lt", 540)]

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
-- last part is so that cases like (1203) do not end up as (12.3) but instead (12.03)

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

formatBill :: BillType -> String
formatBill billList = formatLines billList ++ formatTotal (makeTotal billList)







-- exercise 42 -----------------------------------------------------------------------
makeTotal :: BillType -> Price
makeTotal billList = sum $ map getPrice billList
    where getPrice (b,p) = p
--sum [p | (_,p) <- billList]


-- exercise 45 -----------------------------------------------------------------------
-- HELP how to use map/filter here?
{-look :: Database -> BarCode -> (Name, Price)
look dBase barCode = filter isBarCode dBase
    where isBarCode (b,n,p) = b == barCode-}

look :: Database -> BarCode -> (Name, Price)
look dBase bCode
    | npList == [] = ("Unknown Item", 0)
    | otherwise    = head npList
    where npList = [(n,p) | (b,n,p) <- dBase, bCode == b]


-- exercise 46 -----------------------------------------------------------------------
lookUp :: BarCode -> (Name, Price)
lookUp bCode = look codeIndex bCode


-- exercise 47 -----------------------------------------------------------------------
makeBill :: TillType -> BillType
makeBill bCodeList = map lookUp bCodeList
-- [lookUp bCode | bCode <- bCodeList]


-- exercise 48 -----------------------------------------------------------------------

-- note for 2 bottles of sherry, 1.00 discount. For 4 bottles, 2.00, For 6, 3.00.
makeDiscount :: BillType -> Price
makeDiscount billList = numBottles `div` 2
       where numBottles = sum $ map getSherries billList
             getSherries (n,p) = if n == "Dry Sherry, 1lt" then 1 else 0



formatDiscount :: Price -> String
formatDiscount p = "\nDiscount" ++ replicate diff '.' ++ p' ++ "\n"
                   where diff = lineLength - length (p' ++ "Discount")
                         p' = show p ++ ".00"

formatBill' :: BillType -> String
formatBill' billList = formatLines billList
                    ++ formatDiscount (makeDiscount billList)
                    ++ formatTotal (makeTotal billList)





--printBill :: BillType  -> IO()
--printBill billList = putStrLn $ formatBill' billList