edit :: [Edit] -> String -> String
edit _ [] = []
edit [] xs = xs
edit (Insert c : rest) (x:xs) = c : edit rest (x:xs)
edit (Change c : rest) (x:xs) = c : edit rest xs
edit (Copy : rest) (x:xs) = x : edit rest xs
edit (Delete : rest) (x:xs) = edit rest xs
edit (Kill : rest) xs = []