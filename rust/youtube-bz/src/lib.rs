use std::env;
use regex::Regex;
use levenshtein::levenshtein;

pub struct Config {
    pub mbid: String,
}

impl Config {
    pub fn new(mut args: env::Args) -> Result<Config, &'static str> {
        args.next();
        let mbid = match args.next() {
            Some(mbid) => mbid,
            None => return Err("No MBID release provided"),
        };

        Ok(Config { mbid })
    }
}

pub struct Release {
    pub artist: String,
    pub tracks: Vec<String>
}

impl Release {
    pub async fn new(mbid: &str, client: &reqwest::Client) -> Release {
        // Generating Music Brainz url to fetch
        let url = format!("https://musicbrainz.org/ws/2/release/{}?inc=artists+recordings&fmt=json", mbid);

        // Getting musicbrainz release
        let res = client.get(url).header("User-Agent", "reqwest").send().await.unwrap();
        let body = res.text().await.unwrap();

        // Parsing musicbrainz response
        let v: serde_json::Value = serde_json::from_str(&body).unwrap();
        let artist = v["artist-credit"][0]["name"].as_str().unwrap();
        let track_list = v["media"][0]["tracks"].as_array().unwrap();
        let mut tracks = Vec::new();
        for track in track_list {
            tracks.push(String::from(track["title"].as_str().unwrap()));
        }
        Release { artist: String::from(artist), tracks: tracks}
    }

}

pub struct Video {
    pub title: String,
    pub id: String
}

pub struct SearchResults {
    pub videos: Vec<Video>
}

impl SearchResults {
    pub async fn new(query: &str, client: &reqwest::Client) -> SearchResults {
        // Generating YouTube url to fetch
        let url = format!("https://www.youtube.com/results?search_query={}", query);
        let mut videos = Vec::new();

        // Fetching url
        let res = client.get(url).send().await.unwrap();
        let body = res.text().await.unwrap();
        
        // Fetching yt initial data
        let re = Regex::new(r"(var ytInitialData = )(.*);</script><script").unwrap();
        let caps = re.captures(&body).unwrap();

        // Parsing yt initial data
        let v: serde_json::Value = serde_json::from_str(&caps[2]).unwrap();
        let item_section_renderer = v["contents"]["twoColumnSearchResultsRenderer"]["primaryContents"]["sectionListRenderer"]["contents"][0]["itemSectionRenderer"]["contents"].as_array().unwrap();
        for content in item_section_renderer {
            let content = content.as_object().unwrap();
            if content.contains_key("videoRenderer") == true {
                videos.push(Video { 
                    title: String::from(content["videoRenderer"]["title"]["runs"][0]["text"].as_str().unwrap()),
                    id: String::from(content["videoRenderer"]["videoId"].as_str().unwrap()),
                });
            }
        }
        SearchResults{ videos: videos }
    }
}

pub async fn run(mbid: &str) {
    // Reqwest client
    let client = reqwest::Client::new();
    let release = Release::new(mbid, &client).await;

    for track in release.tracks {
        // Init best distance
        let mut best_distance: Vec<(usize, String)> = Vec::new();

        // Generating YouTube url to fetch
        let query = format!("\"{}\"+\"{}\"+\"{}\"", release.artist, track, "Auto-generated");

        let results = SearchResults::new(&query, &client).await;

        for video in results.videos {
            let distance = levenshtein(&video.title, &track);
            best_distance.push((distance, String::from(video.id)));
            if distance == 0 {
                break
            }
        }
        best_distance.sort_by(|a, b| a.0.cmp(&b.0));
        println!("{}: https://www.youtube.com/watch?v={}", track, best_distance[0].1);
    }

}
