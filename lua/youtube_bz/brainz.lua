#!/usr/bin/env lua
-- brainz.lua
module(..., package.seeall)

-- Require
local http_request = require "http.request"
local json = require "json"

-- Function

---[[ Get Brainz result query
function getBrainzResult(mbid)
        local url = string.format("https://musicbrainz.org/ws/2/release/%s?inc=artists+recordings&fmt=json", mbid)
        local headers, stream = assert(http_request.new_from_uri(url):go())
        local body = assert(stream:get_body_as_string())
        if headers:get ":status" ~= "200" then
            error(body)
            return
        end
        return json.decode(body)
end
--]]
