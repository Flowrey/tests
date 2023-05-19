{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Aeson
import Text.Regex.TDFA
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header (Header, hUserAgent)
import Network.HTTP.Types.Status (statusCode)
import GHC.Generics
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 as C8 (pack, ByteString)
import Control.Lens
import Data.Aeson.Lens

newtype ArtistCredit = ArtistCredit {
    aName :: String
} deriving Show

instance FromJSON ArtistCredit where
    parseJSON = withObject "ArtistCredit" $ \v -> ArtistCredit
        <$> v .: "name"

data Track = Track {
    tTitle :: String
    , tPosition :: Int
    } deriving Show


instance FromJSON Track where
    parseJSON = withObject "Track" $ \v -> Track
        <$> v .: "title"
        <*> v .: "position"

newtype Media = Media {
    mTracks :: [Track]
    } deriving Show

instance FromJSON Media where
    parseJSON = withObject "Media" $ \v -> Media
        <$> v .: "tracks"

data Release = Release {
      rArtistCredit :: [ArtistCredit]
    , rMedia :: [Media]
    , rTitle :: String
    } deriving Show

instance FromJSON Release where
    parseJSON = withObject "Release" $ \v -> Release
        <$> v .: "artist-credit"
        <*> v .: "media"
        <*> v .: "title"

main :: IO ()
main = do
  response <- getMusicBrainzResult "a17a48b6-51db-3c52-8fdd-066fb9989f78"

  case decode (responseBody response) :: Maybe Release of
    Nothing -> putStrLn "Unable to decoode body to JSON structure"
    Just release -> do
        ytReslist <- getSearchResultsForEachQuery $ constructListOfYoutubeSearchQuery release
        let ytInitData = map (getYouTubeInitialData . responseBody) ytReslist
        let decodedYTInitData = map decode ytInitData :: [Maybe Value]
        mapM_ (print . getVideoId) decodedYTInitData


getVideoId :: Maybe Value -> Maybe Value
getVideoId firstDecodeYTInitData = case firstDecodeYTInitData  of
            Nothing -> Nothing
            Just ytData -> ytData ^? key
                    "contents".key
                    "twoColumnSearchResultsRenderer".key
                    "primaryContents".key
                    "sectionListRenderer".key
                    "contents".nth 0.key
                    "itemSectionRenderer".key
                    "contents".nth 0.key
                    "videoRenderer".key
                    "videoId"

getYouTubeInitialData :: BSL.ByteString -> BSL.ByteString
getYouTubeInitialData rawBody =  getAllTextSubmatches (rawBody =~ initialDataRegex)!!2

getSearchResultsForEachQuery :: [String] -> IO [Response BSL.ByteString]
getSearchResultsForEachQuery = mapM getSearchResults

constructListOfYoutubeSearchQuery :: Release -> [String]
constructListOfYoutubeSearchQuery release = map (constructYoutbeSearchQuery . head $ rArtistCredit release) (mTracks . head $ rMedia release)

constructMusicBrainzRequestUrl :: String -> String
constructMusicBrainzRequestUrl mbid = musicBrainzUrl <> mbid

constructYoutbeSearchQuery :: ArtistCredit -> Track -> String
constructYoutbeSearchQuery artist track = aName artist <> " " <> tTitle track <> " " <> "\"Auto-generated\""

initialDataRegex :: String
initialDataRegex = "(var ytInitialData = )(.*);</script><script"

youtubeUrl :: String
youtubeUrl = "https://www.youtube.com"

musicBrainzUrl :: String
musicBrainzUrl = "https://musicbrainz.org/ws/2/release/"

getSearchResults :: String -> IO (Response BSL.ByteString)
getSearchResults search_query = do
    manager <- newManager tlsManagerSettings
    initReq <- parseRequest $ youtubeUrl <> "/results"
    let req = setQueryString [("search_query", Just $ C8.pack search_query)] initReq
    httpLbs req manager

getMusicBrainzResult :: String -> IO (Response BSL.ByteString)
getMusicBrainzResult mbid = do
  manager <- newManager tlsManagerSettings
  initReq <- parseRequest (constructMusicBrainzRequestUrl mbid)
  let request = initReq {
        method = "GET"
        , requestHeaders = [(hUserAgent, "YoutubeBrainz/0.1.0")]
        , queryString = "?inc=artists+recordings&fmt=json"
  }
  httpLbs request manager