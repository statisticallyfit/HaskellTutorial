

onSeperateLines :: [String] -> String
onSeperateLines stringList = concat [word ++ "\n" | word <- stringList]
--[letter | word <- stringList, letter <- word ++ "\n"]


main = do
    putStr $ onSeperateLines ["blue", "whales", "surfing", "the", "waves"]