import Test.QuickCheck
import Test.QuickCheck.Function



functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)





data Sum b a = First a | Second b deriving (Eq, Show)

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b


type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool




main = do
    quickCheck $ \x -> functorIdentity (x :: [Int])
    quickCheck (functorIdentity :: [Int] -> Bool )
    quickCheck (functorCompose :: IntFC)
    putStrLn ""
    --print $ fmap (+1) (First 1)
    --print $ fmap (+2) (Second 4)

    {-
    *Main> fmap (+1) (First 10)
    First 11
    *Main> fmap (+1) (Second 1)
    Second 1
    -}