import Prelude hiding (mapMaybe, maybe)

data Err a = OK a | Error String deriving (Eq, Show)


mapMaybe :: (a -> b) -> Err a -> Err b
mapMaybe g (Error msg) = Error msg
mapMaybe g (OK x) = OK (g x)


-- note type: trapping an error
maybe :: b -> (a -> b) -> Err a -> b
maybe n f (Error _) = n
maybe n f (OK x) = f x


composeMaybe :: (a -> Err b) -> (b -> Err c) -> a -> Err c
composeMaybe f g a = case (f a) of
                        Error msg -> Error msg
                        OK b -> g b -- results in Just c or Nothing

f1 x = OK (x+1) -- how to make end type be b?
f2 x = Error "f2"

--g :: b -> Maybe c
g1 y = OK (y+1) -- how to make end type be c?
g2 y = Error "g2"



main = do
    print $ maybe 56 (1+) (OK 12)
    print $ maybe 56 (1+) (Error "hi")
    print $ composeMaybe f1 g1 1
    print $ mapMaybe (+1) (Error "error")
    print $ mapMaybe (+100) (OK 100)