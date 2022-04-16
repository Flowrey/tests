#include <iostream>
#include <regex>
#include <string>
#include <curl/curl.h>
#include "nlohmann/json.hpp"

using json = nlohmann::json;

size_t writeFunction(void* ptr, size_t size, size_t nmemb, std::string* data) {
    data->append((char*)ptr, size * nmemb);
    return size * nmemb;
}

int main(int argc, char** argv) {
    const std::string base_url = "https://www.youtube.com/results?search_query=";
    const std::regex e("(var ytInitialData = )(.*);</script><script", std::regex::extended);

    std::smatch match;
    std::string payload;
    std::string url;
    std::string response;

    json j_yt_inital_data;
    json itemSectionRenderer;

    CURL* curl = curl_easy_init();

    curl_easy_setopt(curl, CURLoption::CURLOPT_USERAGENT, "curl/7.42.0");
    curl_easy_setopt(curl, CURLoption::CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLoption::CURLOPT_WRITEFUNCTION, writeFunction);
    curl_easy_setopt(curl, CURLoption::CURLOPT_URL, "https://musicbrainz.org/ws/2/release/6c1adf00-edaf-4fe0-9d57-8dc5da90a4a9?inc=artists+recordings&fmt=json");
    curl_easy_perform(curl);
    const json release = json::parse(response);
    
    for (json track : release["media"][0]["tracks"]) {;
        payload = curl_easy_escape(curl, track["title"].dump().c_str(), 0);
        url = base_url + payload;
        curl_easy_setopt(curl, CURLoption::CURLOPT_URL, url.c_str());
        curl_easy_perform(curl);
 
        std::regex_search(response, match, e);
        std::string yt_initial_data = match[2];

        j_yt_inital_data = json::parse(yt_initial_data);
        itemSectionRenderer = j_yt_inital_data["contents"]["twoColumnSearchResultsRenderer"]["primaryContents"]["sectionListRenderer"]["contents"][0]["itemSectionRenderer"]["contents"];
        for (json video : itemSectionRenderer)
            if (video.contains("videoRenderer")) {
                std::cout << video["videoRenderer"]["videoId"] << std::endl;
                std::cout << video["videoRenderer"]["title"]["runs"][0]["text"] << std::endl;
            }
    }
    curl_easy_cleanup(curl);
    return 0;
}
