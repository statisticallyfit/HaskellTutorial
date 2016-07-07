import Data.List
import Prelude hiding (either, eitherMaybe)



--- 1

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False


fromLeft :: Either a b -> a
fromLeft (Left x) = x

fromRight :: Either a b -> b
fromRight (Right x) = x


lefts :: [Either a b] -> [a]
lefts = (map fromLeft) . (filter isLeft)


--- 2
rights :: [Either a b] -> [b]
rights = (map fromRight) . (filter isRight)


--- 3
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers es = ((lefts es), (rights es))


--- 4
-- >> eitherMaybe (+1) (Right 11)  ---> Just 12
-- >> eitherMaybe (+1) (Left 'a')  ---> Nothing
eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe f (Right b) = Just (f b)
eitherMaybe f (Left a)  = Nothing


--- 5
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a)  = f a
either _ g (Right b) = g b

s = Left "moon" :: Either String Int
n = Right 3 :: Either String Int

evalL = either length (*8) s -- note the functions must have SAME result type!
evalR = either length (*8) n



--- 6
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' g etr = Just $ either undefined g etr
