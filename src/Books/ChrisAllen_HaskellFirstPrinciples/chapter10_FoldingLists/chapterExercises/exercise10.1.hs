
--- 1 a)

stops = "pbtdkg"
vowels = "aeiou"

stopsVowelsStops = [(s, v, s') | s <- stops, v <- vowels, s' <- stops]


--- 1 b)
beginWithP = [(s, v, s') | s <- stops, v <- vowels, s' <- stops, s == 'p']


--- 1 c)

nouns = ["dog", "trumpet", "glory", "birch", "cabin", "waves", "watermelon"]
verbs = ["fight", "sing", "unveil", "scavenge", "resurrect", "discover", "fly"]

nounVerbNoun = [(n,v,n') | n <- nouns, v <- verbs, n' <- nouns]






--- 1 a) answers

combosDo :: [a] -> [b] -> [c] -> [(a,b,c)]
combosDo xs ys zs = do
    x <- xs
    y <- ys
    z <- zs
    return (x,y,z)


combosApply :: [a] -> [b] -> [c] -> [(a,b,c)]
combosApply xs ys zs = (,,) <$> xs <*> ys <*> zs
-- note is same as: liftM3 (,,) xs ys zs

combosBind :: [a] -> [b] -> [c] -> [(a,b,c)]
combosBind xs ys zs =
    xs >>=
    (\x -> ys >>=
           (\y -> zs >>=
                  (\z ->
                        return (x,y,z))))

-- help todo understand better
combosCM :: [a] -> [b] -> [c] -> [(a,b,c)]
combosCM xs ys zs =
    concatMap (\x ->
        concatMap (\y ->
            concatMap (\z -> return (x,y,z)) zs) ys) xs


-- help todo understand better.
combosFoldr :: [a] -> [b] -> [c] -> [(a,b,c)]
combosFoldr xs ys zs =
    foldr ((++) . (\x ->
        foldr ((++) . (\y ->
            foldr ((++) . (\z -> return (x,y,z))) [] zs)) [] ys)) [] xs



--- 1 b) answer

fst' :: (a,b,c) -> a
fst' (a,_,_) = a

-- help todo what is the meaning of how this is structured:
-- (==) 'p' . fst'
combosPOnly :: String -> String -> String -> [(Char, Char, Char)]
combosPOnly xs ys zs =
    filter ((==) 'p' . fst') candidates
    where candidates = (,,) <$> xs <*> ys <*> zs