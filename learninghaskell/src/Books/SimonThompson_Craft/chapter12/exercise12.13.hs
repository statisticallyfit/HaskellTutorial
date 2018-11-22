
-- note splits "Spy" ===> [("", "Spy"), ("S", "py"), ("Sp", "y"), ("Spy", "")]
splits :: [a] -> [([a], [a])]
splits xs = [ splitAt n xs | n <- [0 .. (length xs)]]
