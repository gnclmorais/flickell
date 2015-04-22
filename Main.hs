{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import System.Environment
import Network.HTTP
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.HTTP.Conduit

import System.IO.Unsafe

import Text.Parsec.Prim
import Text.Parsec.Token
import Text.Parsec.Combinator
import qualified Text.ParserCombinators.Parsec as P

import Data.Char (isLetter, isDigit)
import qualified Data.Text as T
import Data.Text.Internal
--Data.Text.init
import Data.Aeson
import GHC.Generics
import Control.Monad
import Control.Applicative

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

testString = "https://api.flickr.com/services/rest/?method=flickr.photosets.getPhotos&format=json&api_key=69ebb4baf3a207f0151310929d56731d&photoset_id=72157635564577774"

testJSON :: L.ByteString
testJSON = "{\"foo\": \"bar\"}"

testPhoto :: L.ByteString
testPhoto = "{\"id\":\"9774244001\",\"secret\":\"82ec04b2a2\",\"server\":\"7338\",\"farm\":8,\"title\":\"CSSConf.eu 2013\",\"isprimary\":\"0\",\"ispublic\":1,\"isfriend\":0,\"isfamily\":0}"

testSet :: L.ByteString
testSet = "{\"id\":\"72157635564577774\",\"primary\":\"9774244001\",\"owner\":\"101926979@N03\",\"ownername\":\"m_besser\",\"photo\":[{\"id\":\"9774244001\",\"secret\":\"82ec04b2a2\",\"server\":\"7338\",\"farm\":8,\"title\":\"CSSConf.eu 2013\",\"isprimary\":\"0\",\"ispublic\":1,\"isfriend\":0,\"isfamily\":0},{\"id\":\"9774529423\",\"secret\":\"ffa1ede746\",\"server\":\"7284\",\"farm\":8,\"title\":\"CSSConf.eu 2013\",\"isprimary\":\"0\",\"ispublic\":1,\"isfriend\":0,\"isfamily\":0}],\"page\":1,\"per_page\":500,\"perpage\":500,\"pages\":1,\"total\":\"34\",\"title\":\"CSSconf.eu 2013\"},\"stat\":\"ok\"}"

-- First function example! Use it like:
-- parse jsonWrapper "unknown" "jsonFlickrApi({\"photoset\": \"test\"})"
jsonWrapper :: P.GenParser Char st String
jsonWrapper = P.many (P.noneOf "{")

chopoff :: L.ByteString -> L.ByteString
chopoff str
    | lenStr < lenWrp = str
    | otherwise       = L.init $ L.drop lenWrp str
    where
        lenStr = L.length str
        lenWrp = L.length ("jsonFlickrApi(" :: L.ByteString)

chopoff' :: L.ByteString -> L.ByteString
chopoff' str
    | lenStr < lenWrp = str
    | otherwise       = L.drop lenWrp str
    where
        lenStr = L.length str
        lenWrp = L.length ("jsonFlickrApi" :: L.ByteString)

--parseWrapper :: ParsecT s u m String
--parseWrapper = do
--    x <- P.many (P.noneOf "{")
--    return x

    --between (symbol "{") (symbol "}")

    --e0 <- many1 letter
    --e1 <- char '('
    --return init

--
-- Data types
data Photo = Photo
    { photoid   :: Text
    , secret    :: Text
    , server    :: Text
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

data FlickrResponse = FlickrResponse
    { stat     :: Text
    , photoset :: Photoset
    } deriving (Show, Generic)

--
-- Data types handling
instance FromJSON Photo where
    parseJSON (Object v) =
        Photo <$> v .: "id"
              <*> v .: "secret"
              <*> v .: "server"
              <*> v .: "farm"
              <*> v .: "title"
              <*> v .: "isprimary"
              <*> v .: "ispublic"
              <*> v .: "isfriend"
              <*> v .: "isfamily"
    parseJSON _ = mzero
instance ToJSON Photo where
    toJSON (Photo photoid secret server farm title
                  isprimary ispublic isfriend isfamily) =
        object [ "id"        .= photoid
               , "secret"    .= secret
               , "server"    .= server
               , "farm"      .= farm
               , "title"     .= title
               , "isprimary" .= isprimary
               , "ispublic"  .= ispublic
               , "isfriend"  .= isfriend
               , "isfamily"  .= isfamily
               ]

instance FromJSON Photoset where
    parseJSON (Object o) = do
        id <- o .: "id"
        primary <- o .: "primary"
        owner <- o .: "owner"
        ownername <- o .: "ownername"
        photo <- parseJSON =<< o .: "photo"
        page <- o .: "page"
        per_page <- o .: "per_page"
        perpage <- o .: "perpage"
        pages <- o .: "pages"
        total <- o .: "total"
        title <- o .: "title"
        return $ Photoset id primary owner ownername photo page per_page perpage pages total title
    parseJSON _ = mzero
instance ToJSON Photoset where
    toJSON (Photoset photosetid primary owner ownername photo
                     page per_page perpage pages total name) =
        object [ "id"        .= photosetid
               , "primary"   .= primary
               , "owner"     .= owner
               , "ownername" .= ownername
               , "photo"     .= photo
               , "page"      .= page
               , "per_page"  .= per_page
               , "perpage"   .= perpage
               , "pages"     .= pages
               , "total"     .= total
               , "title"     .= name
               ]

instance FromJSON FlickrResponse where
    parseJSON (Object o) = do
        stat <- o .: "stat"
        photoset <- parseJSON =<< o .: "photoset"
        return $ FlickrResponse stat photoset
    --parseJSON (Object v) =
    --    FlickrResponse <$> v .: "photoset"
    --                   <*> v .: "stat"
    --parseJSON _ = mzero
instance ToJSON FlickrResponse where
    toJSON (FlickrResponse photoset stat) =
        object [ "photoset" .= photoset
               , "stat"     .= stat
               ]

--
-- Makes a simple request to an URL (HTTPS supported)
--request :: String -> IO L.ByteString
--request url = simpleHttp url
request :: String -> L.ByteString
request url = unsafePerformIO $ simpleHttp url

--
-- Test function
--testFunc = do
--    d <- (eitherDecode <$> (request testString)) :: IO (Either String [FlickrResponse])
--    show d

-- TEST - THIS KIND OF WORKS:
-- eitherDecode $ chopoff $ request testString :: Either String FlickrResponse

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
