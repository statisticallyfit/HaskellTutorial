

squashMaybe :: Maybe (Maybe a) -> Maybe a
squashMaybe (Just (Just x)) = Just x
squashMaybe _ = Nothing