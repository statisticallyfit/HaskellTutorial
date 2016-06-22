import Test.QuickCheck


twist :: Either a b -> Either b a
twist (Left x) = Right x
twist (Right y) = Left y




propTwistIdentity :: (Eq a, Eq b) => Either a b -> Bool
propTwistIdentity e = (twist . twist) e == e


main = do
    --quickCheck propTwistIdentity -- help problem showing
    print $ twist $ twist ((Right 10) :: Either String Int) -- case 3
    print $ twist ((Left "string") :: Either String Int) -- case 1
    print $ twist ((Right 3) :: Either String Int) -- case 2