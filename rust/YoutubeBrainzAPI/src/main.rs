use std::str::{self};
use actix_web::{get, App, HttpServer, Responder, web};
use awc::Client;
use serde_json::Value;
use serde::{Serialize,Deserialize};
use urlencoding::encode;
use regex::Regex;
use levenshtein::levenshtein;

#[derive(Serialize)]
struct Video {
    id: String,
    title: String,
    channel: String,
}

#[derive(Serialize)]
struct Match {
    title: String,
    id: String,
    url: String,
}

#[derive(Serialize, Deserialize)]
struct ArtistCredit {
    name: String
}

#[derive(Serialize, Deserialize)]
struct Track {
    title: String
}

#[derive(Serialize, Deserialize)]
struct Media {
    tracks: Vec<Track>,
}

#[derive(Serialize, Deserialize)]
struct Release {
    title: String,
    #[serde(rename = "artist-credit")]
    artist_credit: Vec<ArtistCredit>,
    media: Vec<Media>,
}


async fn get_closest_match(track: &str, videos: Vec<Video>) -> String {
    let mut best_distance = levenshtein(&*videos[0].title, track);
    let mut best_match = &videos[0].id;
    for video in &videos {
        let distance = levenshtein(&*video.title, track);
        if distance < best_distance {
            best_distance = distance;
            best_match = &video.id;
        }
    }
    return best_match.to_string();
}


async fn get_bz_release(mbid: String) -> Result<Release, Box<dyn std::error::Error>> {
    let client = awc::Client::default();
    let mut res = client
        .get(format!("https://musicbrainz.org/ws/2/release/{}?inc=artists+recordings&fmt=json", mbid))
        .insert_header(("User-Agent", "Actix-web"))
        .send().
        await?;
    let b_data = res.body().await?;
    let data = str::from_utf8(&*b_data)?;
    let r: Release = serde_json::from_str(data)?;
    Ok(r)
}

async fn parse_yt_initial_data(yt_initial_data: &str) -> Result<Vec<Video>, Box<dyn std::error::Error>> {
    let mut videos = vec![];
    let v: Value = serde_json::from_str(yt_initial_data).unwrap();
    let item_section_renderer = &v["contents"]["twoColumnSearchResultsRenderer"]["primaryContents"]["sectionListRenderer"]["contents"][0]["itemSectionRenderer"]["contents"];
    for item in item_section_renderer.as_array().unwrap() {
        if !!!item["videoRenderer"].is_null() {
            let video = Video{
                id: String::from(item["videoRenderer"]["videoId"].as_str().unwrap()),
                title: String::from(item["videoRenderer"]["title"]["runs"][0]["text"].as_str().unwrap()),
                channel: String::from(item["videoRenderer"]["longBylineText"]["runs"][0]["text"].as_str().unwrap())
            };
            videos.push(video);
        }
    }
    Ok(videos)
}

async fn get_yt_initial_data(yt_results: String) -> Result<String, Box<dyn std::error::Error>> {
    let re = Regex::new(r"(var ytInitialData = )(.*);</script><script")?;
    let cap = re.captures(&yt_results).unwrap();
    Ok(String::from(&cap[2]))
}

async fn get_yt_results(search_query: String, client: &Client) -> Result<String, Box<dyn std::error::Error>> {
    let r = client.get(format!("https://www.youtube.com/results?search_query={}", search_query))
        .send()
        .await?
        .body()
        .await?;
    let body = str::from_utf8(&r)?;
    Ok(String::from(body))
}

#[get("/release/{mbid}")]
async fn release(mbid: web::Path<String>) -> Result<impl Responder, Box<dyn std::error::Error>>{

    let client = awc::Client::default();

    let mut results = Vec::new();

    let release = get_bz_release(mbid.into_inner()).await?;

    for media in &release.media {
        for track in &media.tracks {

            let query = format!("{} {} {} \"Auto-generated\"", release.artist_credit[0].name, release.title, &track.title);

            let yt_results = get_yt_results(encode(&query).into_owned(), &client).await?;

            let yt_initial_data = get_yt_initial_data(yt_results).await?;

            let videos = parse_yt_initial_data(&yt_initial_data).await?;

            let closest_match = get_closest_match(track.title.as_str(), videos).await;

            let result = Match {
                title: track.title.as_str().parse()?,
                id: closest_match.to_string(),
                url: format!("https://www.youtube.com/watch?v={}", closest_match.to_string()),
            };

            results.push(result);
        }
    }
    Ok(web::Json(results))
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new().service(release)
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}