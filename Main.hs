{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import System.Environment
import Network.HTTP
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.HTTP.Conduit

import Data.Text
import Data.Aeson
import GHC.Generics
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Data.ByteString.Lazy as L

--
-- Data types
data Photo = Photo
    { photoid   :: Text
    , secret    :: Text
    , sever     :: Text
    , farm      :: Int
    , title     :: Text
    , isprimary :: Text
    , ispublic  :: Int
    , isfriend  :: Int
    , isfamily  :: Int
    } deriving (Show, Generic)

data Photoset = Photoset
    { photosetid :: Text
    , primary    :: Text
    , owner      :: Text
    , ownername  :: Text
    , photo      :: [Photo]
    , page       :: Int
    , per_page   :: Int
    , perpage    :: Int
    , pages      :: Int
    , total      :: Text
    , name       :: Text
    } deriving (Show, Generic)

data Response = Response
    { photoset :: Photoset
    , stat     :: Text
    } deriving (Show, Generic)

--
-- Data types handling
--instance FromJSON Response
--instance ToJSON Response

--
-- Finds arguments and their value
findArg :: String -> [String] -> Maybe String
findArg k []  = Nothing
findArg k [_] = Nothing
findArg k (k':a:as) | k == k' = Just a
findArg k (k':a:as) = findArg k as

--
-- Finds simple flags
findFlag :: String -> [String] -> Bool
findFlag f [] = False
findFlag f (f':fs) | f == f' = True
findFlag f (f':fs) = findFlag f fs

--
-- Makes a simple request to an URL (HTTPS supported)
request :: String -> IO ()
request url = simpleHttp url >>= L.putStr

--
-- Downloads the photos
download :: String -> String -> String -> IO Bool
download _ _ _ = return True

main = do
    args <- getArgs

    -- Get the arguments you need: API key, API secret and the Flickr set ID
    let apiKey    = findArg "-k" args
    let apiSecret = findArg "-s" args
    let setId     = findArg "-i" args

    -- Cascade through the arguments to make sure all of them are set
    case apiKey of
        Nothing -> error "No API key provided!"
        Just k  ->
            case apiSecret of
                Nothing -> error "No API secret provided!"
                Just s  ->
                    case setId of
                        Nothing -> error "No set ID provided!"
                        Just i  -> download (show k) (show s) (show i)
