

type Picture = [[Char]]



chessBoard :: Int -> Picture
chessBoard n = splitN n line
    where line = drop (n*n) $ concat $ take (n*n) (repeat "#.")




-- note n = into how many parts of n elements we should split the string
splitN        :: Int -> String -> Picture
splitN _ []   = []
splitN n line = spaced : splitN n (drop n line)
    where spaced = spacify (take n line)

spacify     :: String -> String
spacify bit = trim spaced
    where spaced = concat $ map (\c -> [c] ++ " ") bit
          trim s = dropWhile (== ' ') (reverse s)


-- returns strings separated by newlines
squash :: Picture -> String
squash = concat . map (++ "\n")

drawChessBoard :: Picture -> IO()
drawChessBoard = putStr . squash -- pic here