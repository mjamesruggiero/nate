main = putStrLn "Greetings, what is your name?" >>
       getLine >>=
       (\inpStr -> putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!")
