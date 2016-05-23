import Test.QuickCheck
import Test.QuickCheck.Function



functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)




data Possibly a = LolNope | Yeppers a deriving (Eq, Show)


instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers (f a)


type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool





main = do
    quickCheck $ \x -> functorIdentity (x :: [Int])
    quickCheck (functorIdentity :: [Int] -> Bool )
    quickCheck (functorCompose :: IntFC)
    putStrLn ""
    print $ fmap (*8) (Yeppers 3)
    print $ fmap (+1) LolNope