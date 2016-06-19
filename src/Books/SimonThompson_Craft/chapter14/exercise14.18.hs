import Prelude hiding (either)


either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x) = f x
either f g (Right y) = g y

-- note
applyLeft :: (a -> c) -> Either a b -> c
applyLeft f (Left x) = either f id (Left x) -- HELP what to put instaed of id func?
applyLeft f (Right x) = error "applyLeft applied to Right"
-- either id f (Right x)