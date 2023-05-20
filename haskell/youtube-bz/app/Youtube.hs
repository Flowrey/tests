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
    )
    where

import Data.Aeson
import GHC.Generics

data Runs = Runs {
    text :: String
} deriving (Generic, Show)
instance FromJSON Runs

data Title = Title {
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

data InitData = InitData {
    initContents :: InitContents
} deriving Show
instance FromJSON InitData where
    parseJSON = withObject "InitData" $ \v -> InitData
        <$> v .: "contents"