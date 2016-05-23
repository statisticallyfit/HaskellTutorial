import Test.QuickCheck
import Test.QuickCheck.Function



functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)







data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant q) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)


type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool


-- HELP why do these run 100% even without using the functor instance??
main = do
    quickCheck $ \x -> functorIdentity (x :: [Int])
    quickCheck (functorIdentity :: [Int] -> Bool )
    quickCheck (functorCompose :: IntFC)
    putStrLn ""

    {- NOTE HELP why doesn't work with print?
    *Main> fmap (*3) Finance
    Finance
    *Main> fmap (*3) (Desk 3)
    Desk 3
    *Main> fmap (*3) (Bloor 4)
    Bloor 12
    -}