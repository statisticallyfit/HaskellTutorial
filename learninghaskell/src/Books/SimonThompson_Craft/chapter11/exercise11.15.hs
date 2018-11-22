

{-
NOTE

-------------------------------------------------

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry :: (     a       ->   b   -> c) -> (a,b) -> c


:t (uncurry uncurry)
((a -> b -> c), (a,b)) -> c




-------------------------------------------------

uncurry :: (a -> b -> c) -> (a,b) -> c
curry   :: (  (a,b)      ->   c)  -> a -> b -> c

DOESN"T WORK because curry expects a function in uncurried
form but uncurry doesn't have this form.



But this works:

curry   :: ((a,b) -> c) -> a -> b  -> c
           -----------    --    -------
                |          |       |
uncurry :: (    a       -> b ->    c)   -> (a,b) -> c

:t (uncurry curry)
((a,b) -> c, a) -> (b -> c) ---- HELP is the part (b -> c) equal to c in uncurry?
-}