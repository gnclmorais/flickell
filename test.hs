import System.Environment

-- Finds arguments and their value
findArg :: String -> [String] -> Maybe String
findArg k []  = Nothing
findArg k [_] = Nothing
findArg k (k':a:as) | k == k' = Just a
findArg k (k':a:as) = findArg k as

-- Finds simple flags
findFlag :: String -> [String] -> Bool
findFlag f [] = False
findFlag f (f':fs) | f == f' = True
findFlag f (f':fs) = findFlag f fs

main = do
    args <- getArgs

    let input = findArg "-i" args
    let output = findArg "-o" args
    let isVerbose = findFlag "-verbose" args

    case input of
        Nothing -> putStrLn $ "Need stuf!"
        Just a  -> putStrLn $ "You want input to be " ++ a

    case output of
        Nothing -> putStrLn $ "Need stuf!"
        Just a  -> putStrLn $ "You want output to be " ++ a

    putStrLn $ "You want verbosity to be " ++ (show isVerbose)
