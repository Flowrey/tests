#!/usr/bin/env lua
-- youtube.lua
module(..., package.seeall)

-- Require
local http_request = require "http.request"
local http_util = require "http.util"
local json = require "json"

-- Function

---[[ Get video info from Youtube JSON
local function getVideoInfo(v)
        local video = {}
        if (v.videoRenderer == nil) then
                return
        elseif (v.videoRenderer.ownerBadges == nil) then
                return
        else
                video.title = v.videoRenderer.title.runs[1].text
                video.chanel = v.videoRenderer.longBylineText.runs[1].text
                video.id = v.videoRenderer.videoId
                video.view = v.videoRenderer.viewCountText.simpleText
                video.length = v.videoRenderer.thumbnailOverlays[1].thumbnailOverlayTimeStatusRenderer.text.simpleText
                video.badge = v.videoRenderer.ownerBadges[1].metadataBadgeRenderer.icon.iconType
        end
        return video
end
--]]

---[[ Get Youtube result query
function getYoutubeResult(search_query)
        search_query = search_query:gsub('%&', '')
        search_query = search_query:gsub('?&', '')
        local url = http_util.encodeURI(string.format("https://www.youtube.com/results?search_query=%s", search_query))
        local headers, stream = assert(http_request.new_from_uri(url):go())
        local body = assert(stream:get_body_as_string())
        if headers:get ":status" ~= "200" then
            error(body)
            return
        end
        return body
end
--]]

---[[ Parse Youtube result query
function parseYoutubeResult(body)
        local videoInfo = {}
        _, ytInitialData = string.match(body, "(var\ ytInitialData\ =\ )(.*);</script><script")
        local data = json.decode(ytInitialData).contents.twoColumnSearchResultsRenderer.primaryContents.sectionListRenderer.contents[1].itemSectionRenderer.contents
        for _, v in pairs(data) do
                info = getVideoInfo(v)
                if (info ~= nil) then
                        table.insert(videoInfo, info)
                end
        end
        return videoInfo
end
--]]
