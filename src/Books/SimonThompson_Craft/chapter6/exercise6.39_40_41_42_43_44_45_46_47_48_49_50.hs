
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



-- NOTE: GOAL: convert list of bar codes into a list of name,price pairs

-- exercise 39 -----------------------------------------------------------------------
formatPence :: Price -> String
formatPence p = show pounds ++ "." ++ (pad pence)
                where pounds = p `div` 100
                      pence = p `mod` 100
                      pad rest = if rest < 10
                                 then "0" ++ (show rest)
                                 else show rest
-- last part is so that cases like (1203) do not end up as (12.3) but instead (12.03)



-- exercise 40 -----------------------------------------------------------------------
formatLine :: (Name, Price) -> String
formatLine (n,p) = n ++ (replicate diff '.') ++ p' ++ "\n"
                   where diff = lineLength - (length (n ++ p'))
                         p' = formatPence p



-- exercise 41 -----------------------------------------------------------------------

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



formatLines :: [(Name, Price)] -> String -- BillType -> String
formatLines billList = [line | npTuple <- billList, line <- formatLine npTuple]




-- exercise 42 -----------------------------------------------------------------------
makeTotal :: BillType -> Price
makeTotal billList = sum [p | (_,p) <- billList]




-- exercise 43 -----------------------------------------------------------------------
formatTotal :: Price -> String
formatTotal p = "\nTotal" ++ replicate diff '.' ++ p'
                where diff = lineLength - length (p' ++ "Total")
                      p' = formatPence p



-- exercise 44 -----------------------------------------------------------------------
formatBill :: BillType -> String
formatBill billList = formatLines billList ++ formatTotal (makeTotal billList)



-- exercise 45 -----------------------------------------------------------------------
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
makeBill bCodeList = [lookUp bCode | bCode <- bCodeList]
--fmap lookUp bCodeList


-- exercise 48 -----------------------------------------------------------------------

-- note for 2 bottles of sherry, 1.00 discount. For 4 bottles, 2.00, For 6, 3.00.
makeDiscount :: BillType -> Price
makeDiscount billList = numBottles `div` 2
                        where numBottles = sum [1 | (n,p) <- billList, n == "Dry Sherry, 1lt"]



formatDiscount :: Price -> String
formatDiscount p = "\nDiscount" ++ replicate diff '.' ++ p' ++ "\n"
                   where diff = lineLength - length (p' ++ "Discount")
                         p' = show p ++ ".00"

formatBill' :: BillType -> String
formatBill' billList = formatLines billList
                    ++ formatDiscount (makeDiscount billList)
                    ++ formatTotal (makeTotal billList)




-- exercise 49 -----------------------------------------------------------------------

-- note first removes all and any instances of (n,p) will same barcode.
-- then replaces them in changed database with the given (n,p) and barcode.

testAdd :: Database
testAdd = [ (4719, "Fish Fingers", 121),
            (5643, "Nappies", 1010),
            (3814, "Orange Jelly", 56),
            (1111, "Hula Hoops", 21),
            (5643, "Nappies", 1010),
            (1112, "Hula Hoops (Giant)", 133),
            (5643, "Nappies", 1010),
            (1234, "Dry Sherry, 1lt", 540)]



add :: Database -> BarCode -> (Name, Price) -> Database
add dBase b (n,p) = [(b, n, p)] ++ removeAll dBase b

removeAll :: Database -> BarCode -> Database
removeAll dBase searchBarCode = [(b,n,p) | (b,n,p) <- dBase, searchBarCode /= b]




-- exercise 50 -----------------------------------------------------------------------
makeBill' :: TillType -> BillType
makeBill' tillList = [(n,p) | (n,p) <- billList, n /= "Unknown Item"]
                     where billList = [lookUp bCode | bCode <- tillList]
-- help why doesn't this work: filter (/= ("Unknown item", 0)) billList

showBill :: BillType  -> IO()
showBill billList = putStrLn $ formatBill' billList