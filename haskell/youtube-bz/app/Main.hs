{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL

import Network.HTTP.Client
import Data.Aeson


import Youtube (InitContents
  , InitData
  , getSearchResults
  , getYouTubeInitialData
  , getSearchResults
  , getYouTubeInitialData
  , runs
  , title
  , videoRenderer
  , videoId
  , isrContents
  , slrContents
  , initContents
  , sectionListRenderer
  , itemSectionRenderer
  , text
  , twoColumnSearchResultsRenderer
  , primaryContents, VideoRenderer
  )


import Musicbrainz (Release, ArtistCredit, Track, aName, tTitle, rMedia, rArtistCredit, mTracks, getMusicBrainzResult)
import Data.Maybe
import GHC.IO.Device (IODevice(tell))

data YoutuBrainzRes = YoutubeBrainzRes {
  ybLevenshtein :: Float,
  ybId :: String,
  ybTitle :: String
} deriving (Show)


main :: IO ()
main = do
  response <- getMusicBrainzResult "a17a48b6-51db-3c52-8fdd-066fb9989f78"

  case decode (responseBody response) :: Maybe Release of
    Nothing -> putStrLn "Unable to decoode body to JSON structure"
    Just release -> do
        ytReslist <- getSearchResultsForEachQuery $ constructListOfYoutubeSearchQuery release
        let ytInitData = map (getYouTubeInitialData . responseBody) ytReslist

        let maybeDecodedYTInitData = map decode ytInitData :: [Maybe InitData]
        let decodeYTInitData = catMaybes maybeDecodedYTInitData
        let isr = mapMaybe (itemSectionRenderer.
                            head.
                            slrContents.
                            sectionListRenderer.
                            primaryContents.
                            twoColumnSearchResultsRenderer.
                            initContents
                            ) decodeYTInitData
        let videoRenderList =  map (mapMaybe videoRenderer . isrContents) isr

        let res = map toYoutubeBrainzRes videoRenderList
        mapM_ (mapM_ print) res

toYoutubeBrainzRes :: [VideoRenderer] -> [YoutuBrainzRes]
toYoutubeBrainzRes = map (\el -> YoutubeBrainzRes {
                                  ybLevenshtein = 0.0,
                                  ybId=videoId el,
                                  ybTitle= Youtube.text $ head $ runs $ title el
                                }
                          )

getSearchResultsForEachQuery :: [String] -> IO [Response BSL.ByteString]
getSearchResultsForEachQuery = mapM getSearchResults

constructListOfYoutubeSearchQuery :: Release -> [String]
constructListOfYoutubeSearchQuery release = map (constructYoutbeSearchQuery . head $ rArtistCredit release) (mTracks . head $ rMedia release)

constructYoutbeSearchQuery :: ArtistCredit -> Track -> String
constructYoutbeSearchQuery artist track = aName artist <> " " <> tTitle track <> " " <> "\"Auto-generated\""