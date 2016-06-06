
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

-- exercise 39
formatPence :: Price -> String
formatPence p = show pounds ++ "." ++ (pad pence)
                where pounds = p `div` 100
                      pence = p `mod` 100
                      pad rest = if rest < 10
                                 then "0" ++ (show rest)
                                 else show rest
-- last part is so that cases like (1203) do not end up as (12.3) but instead (12.03)



-- exercise 40
formatLine :: (Name, Price) -> String
formatLine (n,p) = n ++ (replicate diff '.') ++ p' ++ "\n"
                   where diff = lineLength - (length (n ++ p'))
                         p' = formatPence p




--formatBill :: BillType -> String