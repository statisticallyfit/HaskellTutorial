
-- note if it is Left a | Right b then make it into Left b | Right a
twist :: Either a b -> Either b a
twist (Left x) = Right x
twist (Right y) = Left y



main = do
    print $ twist ((Left "string") :: Either String Int)
    print $ twist ((Right 3) :: Either String Int)
    print $ twist $ twist ((Right 10) :: Either String Int)
