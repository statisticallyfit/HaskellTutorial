
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

