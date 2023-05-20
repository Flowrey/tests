{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL

import Network.HTTP.Client
import Data.Aeson


import Youtube (InitContents, InitData, getSearchResults, getYouTubeInitialData)
import Musicbrainz (Release, ArtistCredit, Track, aName, tTitle, rMedia, rArtistCredit, mTracks, getMusicBrainzResult)

main :: IO ()
main = do
  response <- getMusicBrainzResult "a17a48b6-51db-3c52-8fdd-066fb9989f78"

  case decode (responseBody response) :: Maybe Release of
    Nothing -> putStrLn "Unable to decoode body to JSON structure"
    Just release -> do
        ytReslist <- getSearchResultsForEachQuery $ constructListOfYoutubeSearchQuery release
        let ytInitData = map (getYouTubeInitialData . responseBody) ytReslist
        
        let firstData = head ytInitData
        let decodedYTInitData = map decode ytInitData :: [Maybe InitData]
        mapM_ print decodedYTInitData


getSearchResultsForEachQuery :: [String] -> IO [Response BSL.ByteString]
getSearchResultsForEachQuery = mapM getSearchResults

constructListOfYoutubeSearchQuery :: Release -> [String]
constructListOfYoutubeSearchQuery release = map (constructYoutbeSearchQuery . head $ rArtistCredit release) (mTracks . head $ rMedia release)

constructYoutbeSearchQuery :: ArtistCredit -> Track -> String
constructYoutbeSearchQuery artist track = aName artist <> " " <> tTitle track <> " " <> "\"Auto-generated\""