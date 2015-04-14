-- Command line arguments/flags:
-- https://wiki.haskell.org/Command_line_option_parsers

-- Good GetOpt example
-- https://wiki.haskell.org/High-level_option_handling_with_GetOpt
 
--import qualified System.Console.GetOpt as O
import System.IO
import Control.Monad
import System.Exit
import System.Environment
import System.Console.GetOpt

data Flag
    = Verbose    -- no arguments
    | Version    -- no arguments
    | APIKey     -- mandatory argument
    | APISecret  -- mandatory argument
    | SetID      -- mandatory argument

data Options = Options
    { optVerbose :: Bool
    , optInput   :: IO String
    , optOutput  :: String -> IO ()
    }

startOptions :: Options
startOptions = Options
    { optVerbose = False
    , optInput   = getContents
    , optOutput  = putStr
    }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"]
        (ReqArg
            (\arg opt -> return opt { optInput = readFile arg })
            "FILE")
        "Input file"

    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = writeFile arg })
            "FILE")
        "Output file"

    , Option "s" ["string"]
        (ReqArg
            (\arg opt -> return opt { optInput = return arg })
            "FILE")
        "Input string"

    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"

    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.01"
                exitWith ExitSuccess))
        "Print version"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

main = do
    args <- getArgs

    putStrLn (show args)

    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args

    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions

    let Options { optVerbose = verbose
                , optInput = input
                , optOutput = output   } = opts

    when verbose (hPutStrLn stderr "Hello!")

    input >>= output
