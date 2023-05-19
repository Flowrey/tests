#!/usr/bin/env lua

require "youtube"
require "brainz"
require "levenshtein"

local function displayYoutubeResult(v)
    print(v.title)
    print(v.chanel)
    print(v.id)
    print(v.view)
    print(v.length)
    print(v.badge)
end

local function getClosestMatch(track, videoInfo)
    best_distance = string.levenshtein(videoInfo[1].title, track.title)
    best_match = videoInfo[1]
    for k, v in pairs(videoInfo) do
        distance = string.levenshtein(v.title, track.title)
        if distance < best_distance then
            best_distance = distance
            best_match = v
        end
    end
    return best_match
end

local mbid = arg[1]
if mbid == nil then
    print(string.format("youtube-bz: %s: no mbid provided", arg[0]))
    os.exit()
end

local releaseInfo = brainz.getBrainzResult(mbid)

for _, track in pairs(releaseInfo.media[1].tracks) do
    local query_string = string.format("\"%s\" \"%s\" \"%s\" \"Auto-generated\"", releaseInfo['artist-credit'][1].name,
    releaseInfo.title, track.title)
    local r_youtube = youtube.getYoutubeResult(query_string)
    local videoInfo = youtube.parseYoutubeResult(r_youtube)
    if table.getn(videoInfo) ~= 0 then
        print(string.format("https://www.youtube.com/watch?v=%s", getClosestMatch(track, videoInfo).id))
    end
end
