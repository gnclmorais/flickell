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

download :: String -> String -> String -> IO Bool
download _ _ _ = return True

main = do
    args <- getArgs

    -- Get the arguments you need: API key, API secret and the Flickr set ID
    let apiKey    = findArg "-k" args
    let apiSecret = findArg "-s" args
    let setId     = findArg "-i" args

    case apiKey of
        Nothing -> error "No API key provided!"
        Just k  ->
            case apiSecret of
                Nothing -> error "No API secret provided!"
                Just s  ->
                    case setId of
                        Nothing -> error "No set ID provided!"
                        Just i  -> download (show k) (show s) (show i)
