{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import System.Environment
import Network.HTTP
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.HTTP.Conduit
import Network.URI (parseURI)

import Debug.Trace

import System.IO.Unsafe

import Data.Either
import Text.Parsec.Prim
import Text.Parsec.Token
import Text.Parsec.Combinator
import qualified Text.ParserCombinators.Parsec as P

import Data.Char (isLetter, isDigit)
import qualified Data.Text as T
import Data.Text.Internal
import Data.Aeson
import GHC.Generics
import Control.Monad
import Control.Applicative

import qualified Data.List.Split as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

testString = "https://api.flickr.com/services/rest/?method=flickr.photosets.getPhotos&format=json&api_key=69ebb4baf3a207f0151310929d56731d&photoset_id=72157635564577774"

testString' = "https://api.flickr.com/services/rest/?method=flickr.photosets.getPhotos&format=json&api_key=69ebb4baf3a207f0151310929d56731d&photoset_id="

testJSON :: L.ByteString
testJSON = "{\"foo\": \"bar\"}"

testPhoto :: L.ByteString
testPhoto = "{\"id\":\"9774244001\",\"secret\":\"82ec04b2a2\",\"server\":\"7338\",\"farm\":8,\"title\":\"CSSConf.eu 2013\",\"isprimary\":\"0\",\"ispublic\":1,\"isfriend\":0,\"isfamily\":0}"

testSet :: L.ByteString
testSet = "{\"id\":\"72157635564577774\",\"primary\":\"9774244001\",\"owner\":\"101926979@N03\",\"ownername\":\"m_besser\",\"photo\":[{\"id\":\"9774244001\",\"secret\":\"82ec04b2a2\",\"server\":\"7338\",\"farm\":8,\"title\":\"CSSConf.eu 2013\",\"isprimary\":\"0\",\"ispublic\":1,\"isfriend\":0,\"isfamily\":0},{\"id\":\"9774529423\",\"secret\":\"ffa1ede746\",\"server\":\"7284\",\"farm\":8,\"title\":\"CSSConf.eu 2013\",\"isprimary\":\"0\",\"ispublic\":1,\"isfriend\":0,\"isfamily\":0}],\"page\":1,\"per_page\":500,\"perpage\":500,\"pages\":1,\"total\":\"34\",\"title\":\"CSSconf.eu 2013\"},\"stat\":\"ok\"}"

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

--
-- Data types
data Size = Size
    { label  :: Text
    , source :: Text
    , url    :: Text
    , media  :: Text
    } deriving (Show, Generic)

data Sizes = Sizes
    { canblog     :: Int
    , canprint    :: Int
    , candownload :: Int
    , size        :: [Size]
    } deriving (Show, Generic)

data PhotoSizes = PhotoSizes
    { sizes :: Sizes
    , stats :: Text
    } deriving (Show, Generic)

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
    { photoset :: Photoset
    , stat     :: Text
    } deriving (Show, Generic)

--
-- Data types handling
instance FromJSON Size where
    parseJSON (Object v) =
        Size <$> v .: "label"
             <*> v .: "source"
             <*> v .: "url"
             <*> v .: "media"
    parseJSON _ = mzero
instance ToJSON Size where
    toJSON (Size label source url media) =
        object [ "label"  .= label
               , "source" .= source
               , "url"    .= url
               , "media"  .= media
               ]

instance FromJSON Sizes where
    parseJSON (Object v) =
        Sizes <$> v .: "canblog"
              <*> v .: "canprint"
              <*> v .: "candownload"
              <*> v .: "size"
    parseJSON _ = mzero
instance ToJSON Sizes where
    toJSON (Sizes canblog canprint candownload size) =
        object [ "canblog"     .= canblog
               , "canprint"    .= canprint
               , "candownload" .= candownload
               , "size"        .= size
               ]

instance FromJSON PhotoSizes where
    parseJSON (Object v) =
        PhotoSizes <$> v .: "sizes"
                   <*> v .: "stat"
    parseJSON _ = mzero
instance ToJSON PhotoSizes where
    toJSON (PhotoSizes sizes stats) =
        object [ "sizes" .= sizes
               , "stat"  .= stats
               ]





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
    parseJSON (Object o) =
        Photoset <$> o .: "id"
                 <*> o .: "primary"
                 <*> o .: "owner"
                 <*> o .: "ownername"
                 <*> o .: "photo"
                 <*> o .: "page"
                 <*> o .: "per_page"
                 <*> o .: "perpage"
                 <*> o .: "pages"
                 <*> o .: "total"
                 <*> o .: "title"
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
    parseJSON (Object v) =
        FlickrResponse <$> v .: "photoset"
                       <*> v .: "stat"
    parseJSON _ = mzero
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
-- TODO Replace `unsafePerformIO`

jsonToData :: String -> Either String FlickrResponse
jsonToData url = do
  d <- eitherDecode $ chopoff $ request testString :: Either String FlickrResponse
  return d

getPhotos :: FlickrResponse -> [Photo]
getPhotos (FlickrResponse (Photoset _ _ _ _ photos _ _ _ _ _ _) _) = photos

--handleJsonFailure :: String -> String
handleJsonFailure msg = do
    return "Failure"

--handleJsonSuccess :: FlickrResponse -> String
handleJsonSuccess rsp = do
    let photoset = getPhotos rsp
    let sizesUrl = map getPhotoSizesUrl photoset
    let sizes = map (getPhotoSizes . T.unpack) sizesUrl
    let urls = map handleSizes sizes
    --downloadPhoto $ head urls
    let ops = map downloadPhoto urls
    sequence_ ops
    return "Done!"
    --True

handleSizes :: Either String PhotoSizes -> Text
handleSizes (Left err) = T.pack err
handleSizes (Right (PhotoSizes (Sizes _ _ _ sizes) _)) =
    Main.source . head $ filter (\x -> Main.label x == "Original") sizes

getPhotoSizes :: String -> Either String PhotoSizes
getPhotoSizes url =
    eitherDecode $ chopoff $ request url :: Either String PhotoSizes


--downloady = do
--    jpg <- get "http://www.irregularwebcomic.net/comics/irreg2557.jpg"
--    B.writeFile "irreg2557.jpg" jpg
--    where
--        get url = let uri = case parseURI url of
--                              Nothing -> error $ "Invalid URI: " ++ url
--                              Just u -> u in
--                  simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody

-- Inspired by http://stackoverflow.com/a/11514868/590525
downloadPhoto :: Text -> IO ()
downloadPhoto url = do
    jpg <- get $ T.unpack $ (T.replace "https" "http" url)
    B.writeFile filename jpg
    where
        --filename = "9774244001_82ec04b2a2_s.jpg"
        filename = T.unpack $ last $ T.split (== '/') url
        get url = simpleHTTP (defaultGETRequest_ $ uri) >>= getResponseBody
            where
                uri = case parseURI url of
                    Nothing -> error $ "Invalid URI: " ++ url
                    Just u -> u




replace old new = join new . S.split old



-- TODO
getPhotoSizesUrl :: Photo -> Text
getPhotoSizesUrl (Photo id _ _ _ _ _ _ _ _) = T.append "https://api.flickr.com/services/rest/?method=flickr.photos.getSizes&format=json&api_key=69ebb4baf3a207f0151310929d56731d&photo_id=" id

-- Given an URL from the method flickr.photos.getSizes, returns the JSON string
fetchPhotoSizes :: Text -> L.ByteString
fetchPhotoSizes url = request $ show url


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


main = do
    args <- getArgs

    -- Get the arguments you need: API key, API secret and the Flickr set ID
    --let apiKey    = findArg "-k" args
    --let apiSecret = findArg "-s" args
    --let setId     = findArg "-i" args

    -- Cascade through the arguments to make sure all of them are set
    --case apiKey of
    --    Nothing -> error "No API key provided!"
    --    Just k  ->
    --        case apiSecret of
    --            Nothing -> error "No API secret provided!"
    --            Just s  ->
    --                case setId of
    --                    Nothing -> error "No set ID provided!"
    --                    Just i  -> download (show k) (show s) (show i)

    -- Get the photoset ID
    let photosetId = testString' ++ "72157635564577774"
    -- Get the photos from it
    let response = jsonToData photosetId
    either (handleJsonFailure) (handleJsonSuccess) response
