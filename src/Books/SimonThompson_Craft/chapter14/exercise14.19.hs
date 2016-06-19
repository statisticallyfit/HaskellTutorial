

applyLeft :: (a -> b) -> a -> Either b c
applyLeft f x = Left (f x)
--applyLeft _ _ = Right (Nothing)

applyRight :: (a -> b) -> a -> Either c b
--applyRight _ _ = Left (Nothing)
applyRight f x = Right (f x)


-- HELP is there a way to define a left for applyright and vice versa?