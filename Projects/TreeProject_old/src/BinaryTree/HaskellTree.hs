import Data.Tree



tree :: Tree String
tree = Node "hello" [ Node "foo" []
                    , Node "bars" [ Node "oi!" []
                                   , Node "baz" [ Node "a" [ Node "b" []
                                                           , Node "c" []]
                                                , Node "d" [ Node "e" []]]]
                     , Node "foobar" []]

t1 = Node "4" [Node "2" [Node "1" [], Node "3" []],
               Node "6" [Node "5" [], Node "7" []]]

t3 = Node "5" [Node "3" [Node "1" [], Node "4" []],
               Node "13" [Node "7" [], Node "14" [Node "17" []]]]


main = do
    putStrLn $ drawTree tree
    putStrLn ""
    putStrLn $ drawTree t1
    putStrLn ""
    putStrLn $ drawTree t3