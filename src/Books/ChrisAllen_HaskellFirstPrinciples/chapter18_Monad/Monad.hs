import Control.Applicative ((*>))
import Control.Monad (join)


sequencing :: IO()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"


-- desugared version 1
sequencing' :: IO()
sequencing' =
    putStrLn "blah" >>
    putStrLn "another thing"

-- desugared version 2
sequencing'' :: IO()
sequencing'' =
    putStrLn "blah" *>
    putStrLn "another thing"


-------------------------------------------------------------------------------------
binding :: IO()
binding = do
    putStr "Input: "
    name <- getLine
    putStrLn name

binding' :: IO()
binding' =
    getLine >>= putStrLn
    -- note: this means instead of naming the variable, we use >>=
    -- to pass it directly.

-------------------------------------------------------------------------------------

-- note: Monad is important for its join.
-- note: join merges the two IO effects
-- notes: goes from String -> IO -> IO(IO())
-- to: String -> IO() -- note: it merges the two IOs
h :: IO (IO ()) -- note: outer IO is for getLine, inner IO is for putStrLn
h = putStrLn <$> getLine
-- now
h' :: IO()
h' = join $ putStrLn <$> getLine

-------------------------------------------------------------------------------------
-- note: desugar do syntax

bindingAndSequencing :: IO()
bindingAndSequencing = do
    putStrLn "name pls: "
    name <- getLine
    putStrLn ("y helo thar: " ++ name)


bindingAndSequencing' :: IO()
bindingAndSequencing' =
    putStrLn "name pls: " >>
    getLine >>= -- note: bind operator should be read as do f1 then f2 and arg is passed indirectly.
    \name -> putStrLn ("y helo thar: " ++ name)




twoBinds :: IO()
twoBinds = do
    putStrLn "name pls: "
    name <- getLine
    putStrLn "age pls: "
    age <- getLine
    putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")


twoBinds' :: IO()
twoBinds' =
    putStrLn "name pls: " >>
    getLine >>=
    \name ->                  -- note: need to do \name and \age separately since we
    putStrLn "age pls: " >>   -- must mark down the user input else it would be
    getLine >>=               -- covered by the next input.
    \age ->
    putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")



twoBinds'' :: IO()
twoBinds'' =
    putStrLn "name pls: " >>
    getLine >>=
        (\name ->                  -- note: need to do \name and \age separately since we
        putStrLn "age pls: " >>   -- must mark down the user input else it would be
        getLine >>=               -- covered by the next input.
        (\age ->
        putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")))



------------------------------------------------------------------------------------
-- 18.4 Examples of Monad use

-- LIST -----------------------------------------------------------------------------
{-
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>=) ::            [a] -> (a -> [b]) -> [b]

return :: Monad m => a -> m a
return ::            a -> [a]
-}
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x then [x^2, x^2] else [x^2]

-- note: the do hides the join of the List monad
twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    x <- xs
    if even x then [x^2, x^2] else []


d xs = [[x^2,x^2] | x <- xs, even x]






-- MAYBE ---------------------------------------------------------------------------
{-
(>>=) :: Monad m => m a -> (a ->     m b) ->     m b
(>>=) ::        Maybe a -> (a -> Maybe b) -> Maybe b

return :: Monad m => a ->     m a
return ::            a -> Maybe a
-}


data Cow = Cow {
    name   :: String,
    age    :: Int,
    weight :: Int
} deriving (Eq, Show)

noEmpty     :: String -> Maybe String
noEmpy ""   = Nothing
noEmpty str = Just str

noNegative               :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing

weightCheck   :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
        in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
    case noEmpty name' of
    Nothing    -> Nothing
    Just nammy ->
        case noNegative age' of
            Nothing   -> Nothing
            Just agey ->
                case noNegative weight' of
                    Nothing      -> Nothing
                    Just weighty ->
                        weightCheck (Cow nammy agey weighty)


-- note: cleaning up function with monad
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
    nammy   <- noEmpty name'
    agey    <- noNegative age'
    weighty <- noNegative weight'
    weightCheck (Cow nammy agey weighty)


mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' = do
    noEmpty name' >>=
        \nammy ->
        noNegative age' >>=
            \agey ->
            noNegative weight' >>=
                \weighty ->
                weightCheck (Cow nammy agey weighty)


main = do
    print $ d [1..10]
    print $ join $ d [1..10]
    (print . join . d) [1..10]
    putStrLn ""
    --------------------------------------
    print $ mkSphericalCow "Bess" 5 499
    print $ mkSphericalCow "Bess" 5 500

    print $ mkSphericalCow' "Bess" 5 499
    print $ mkSphericalCow' "Bess" 5 500