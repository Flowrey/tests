{-# LANGUAGE OverloadedStrings #-}

module Musicbrainz 
    ( ArtistCredit
    , Track
    , Media
    , Release
    , rArtistCredit
    , mTracks
    , rMedia
    , aName
    , tTitle
    , getMusicBrainzResult
    ) where
import Data.Aeson

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header (Header, hUserAgent)

import qualified Data.ByteString.Lazy as BSL

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


musicBrainzUrl :: String
musicBrainzUrl = "https://musicbrainz.org/ws/2/release/"

constructMusicBrainzRequestUrl :: String -> String
constructMusicBrainzRequestUrl mbid = musicBrainzUrl <> mbid

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