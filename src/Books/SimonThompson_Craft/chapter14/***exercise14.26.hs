

--f :: a -> Maybe b
f1 x = Just (x+1) -- how to make end type be b?
f2 x = Nothing

--g :: b -> Maybe c
g1 y = Just (y+1) -- how to make end type be c?
g2 y = Nothing




-- method 1

composeMaybe :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
composeMaybe f g a = case (f a) of
                        Nothing -> Nothing
                        Just b -> g b -- results in Just c or Nothing

-----------------------------------------------------------------------

-- method 2

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe g Nothing = Nothing
mapMaybe g (Just x) = Just (g x)

squashMaybe :: Maybe (Maybe a) -> Maybe a
squashMaybe (Just (Just x)) = Just x
squashMaybe _ = Nothing

{-
HELP how to define using above two functions?
composeMaybe' :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
composeMaybe' f g -}
