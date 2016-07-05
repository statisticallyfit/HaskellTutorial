


--- 2
-- note gets sum of lengths of all words and divides that by amount of words.
-- this is average length of a word.
averageWordLen x = (sum (map length (words x))) `div`
                   (length (words x))

preciseAverageWordLen x = fromIntegral (sum (map length (words x))) /
                          fromIntegral (length (words x))
