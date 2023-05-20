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
    ) where
import Data.Aeson

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