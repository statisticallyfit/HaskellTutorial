import Types (Tree (Leaf, Node), Bit (L,R), HuffmanCode, Table)




-- note look up each char in the tbale and concatenate the results
codeMessage :: Table -> String -> HuffmanCode
codeMessage table = concat . map (lookupTable table) -- the arg of char list here

-- loooking up the value n corresponding to  a ke.y char.
lookupTable :: Table -> Char -> HuffmanCode
lookupTable [] searchChar = error "lookupTable"
lookupTable ((char, hcode) : tableRest) searchChar
    | char == searchChar = hcode
    | otherwise = lookupTable tableRest searchChar



table1 = [('a', [L]), ('b', [R,L]), ('t', [R,R])]

main = do
    print $ codeMessage table1 "battab"


    {-
    NOTE evaluation

    codeMessage table1 "battab"
    =  -->
    = concat $ map (lookupTable table1) "battab"
        = lookupTable table1 'b'
        = lookupTable (('a',[L]) : rest) 'b'
        = lookupTable (('b',[R,L]) : rest) 'b'
        = [R,L]

        = lookupTable table1 'a' = [L]
        = lookupTable table1 't' = [R,R]
        = lookupTable table1 't' = [R,R]
        = lookupTable table1 'a' = [L]
        = lookupTable table1 't' = [R,R]

     => concat $ [[R,L], [L], [R,R], [R,R], [L], [R,R]]
     = [R,L, L, R, R, R, R, L, R, R]
    -}