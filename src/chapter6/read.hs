main = do
        putStrLn "Please enter a double:"
        inputStr <- getLine
        let inDouble = (read inputStr)::Double
        putStrLn ("Twice " ++ show inDouble ++ " is  " ++ show (inDouble * 2))
