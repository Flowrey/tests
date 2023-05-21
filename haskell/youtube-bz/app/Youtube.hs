{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Youtube
    (VideoRenderer
    , ISRContent
    , ItemSectionRenderer
    , SLRContent
    , SectionListRenderer
    , PrimaryContents
    , InitContents
    , InitData
    , getSearchResults
    , getYouTubeInitialData
    , runs
    , title
    , text
    , videoRenderer
    , videoId
    , isrContents
    , slrContents
    , initContents
    , sectionListRenderer
    , itemSectionRenderer
    , twoColumnSearchResultsRenderer
    , primaryContents
    )
    where

import Data.Aeson
import GHC.Generics

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header (Header, hUserAgent)

import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 as C8 (pack, ByteString)

import Text.Regex.TDFA


newtype Runs = Runs {
    text :: String
} deriving (Generic, Show)
instance FromJSON Runs

newtype Title = Title {
    runs :: [Runs]
} deriving (Generic, Show)
instance FromJSON Title

data VideoRenderer = VideoRenderer {
    videoId :: String
    ,title :: Title
} deriving (Generic, Show)
instance FromJSON VideoRenderer

newtype ISRContent = ISRContent {
    videoRenderer :: Maybe VideoRenderer
} deriving (Generic, Show)
instance FromJSON ISRContent


newtype ItemSectionRenderer = ItemSectionRenderer {
    isrContents :: [ISRContent]
} deriving Show
instance FromJSON ItemSectionRenderer where
    parseJSON = withObject "ItemSectionRenderer" $ \v -> ItemSectionRenderer
        <$> v .: "contents"

newtype SLRContent = SLRContent {
    itemSectionRenderer :: Maybe ItemSectionRenderer
} deriving (Generic, Show)
instance FromJSON SLRContent

newtype SectionListRenderer = SectionListRenderer {
    slrContents :: [SLRContent]
} deriving Show
instance FromJSON SectionListRenderer where
    parseJSON = withObject "SectionListRenderer" $ \v -> SectionListRenderer
        <$> v .: "contents"

newtype PrimaryContents = PrimaryContents {
    sectionListRenderer :: SectionListRenderer
} deriving (Generic, Show)
instance FromJSON PrimaryContents

newtype TwoColumnSearchResultsRenderer = TwoColumnSearchResultsRenderer {
    primaryContents :: PrimaryContents
} deriving (Generic, Show)
instance FromJSON TwoColumnSearchResultsRenderer

newtype InitContents = InitContents {
    twoColumnSearchResultsRenderer :: TwoColumnSearchResultsRenderer
} deriving (Generic, Show)
instance FromJSON InitContents

newtype InitData = InitData {
    initContents :: InitContents
} deriving Show
instance FromJSON InitData where
    parseJSON = withObject "InitData" $ \v -> InitData
        <$> v .: "contents"


youtubeUrl :: String
youtubeUrl = "https://www.youtube.com"

initialDataRegex :: String
initialDataRegex = "(var ytInitialData = )(.*);</script><script"

getYouTubeInitialData :: BSL.ByteString -> BSL.ByteString
getYouTubeInitialData rawBody =  getAllTextSubmatches (rawBody =~ initialDataRegex)!!2


getSearchResults :: String -> IO (Response BSL.ByteString)
getSearchResults search_query = do
    manager <- newManager tlsManagerSettings
    initReq <- parseRequest $ youtubeUrl <> "/results"
    let req = setQueryString [("search_query", Just $ C8.pack search_query)] initReq
    httpLbs req manager