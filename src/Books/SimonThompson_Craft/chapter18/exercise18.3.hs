import Prelude hiding (repeat)

-- HELP how to test this? How to get it to stop repeating?
-- how to make condition testIO return True at some point? todo 
repeat :: IO Bool -> IO() -> IO()
repeat testIO oper = do test <- testIO
                        if not test
                        then
                            do oper
                               repeat testIO oper
                        else return ()


anIOTest :: IO Bool
anIOTest = return (\n -> if n == 20 then True else False)