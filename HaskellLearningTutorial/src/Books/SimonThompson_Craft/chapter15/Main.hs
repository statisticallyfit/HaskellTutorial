import Types    (Tree (Leaf, Node), Bit(L,R), HuffmanCode, Table)
import Coding   (codeMessage, decodeMessage)
import MakeCode (codes, codeTable)


{-
codeMessage :: Table -> String -> HuffmanCode
decodeMessage :: Tree -> HuffmanCode -> String
codes :: [Char] -> Tree
codeTable :: Tree -> Table
-}


textToBeCoded :: String
textToBeCoded = "there is a green hill"

code :: HuffmanCode
code = codeMessage table textToBeCoded

tree :: Tree
tree = codes textToBeCoded

table :: Table
table = codeTable tree

decoded :: String
decoded = decodeMessage tree code





