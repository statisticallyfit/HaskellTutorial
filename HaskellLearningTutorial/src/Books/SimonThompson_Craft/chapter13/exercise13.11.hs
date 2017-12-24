

showBoolFun :: (Bool -> Bool) -> String
showBoolFun f = "value:    | f (value): "  ++ "\n"
             ++ "-------------------------"  ++ "\n"
             ++ "True      | " ++ (show $ f True) ++ "\n"
             ++ "False     | " ++ (show $ f False)  ++ "\n"



showBoolFunGen :: (a -> String) -> (Bool -> a) -> String
showBoolFunGen fs fb =
     "value:    | f (value): "  ++ "\n"
  ++ "-------------------------"  ++ "\n"
  ++ "True      | " ++ ((fs . fb) True) ++ "\n"
  ++ "False     | " ++ ((fs . fb) False)  ++ "\n"


printFun result = putStr result


main = do
    printFun $ showBoolFun (\x -> if x == True then False else True)
    putStrLn ""
    printFun $ showBoolFunGen show (\b -> if b == True then 33 else 107)