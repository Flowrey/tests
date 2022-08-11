use std::env;
use std::process;

use youtube_bz::Config;

#[tokio::main]
async fn main() {
    // Parse args
    let config = Config::new(env::args()).unwrap_or_else(|err| {
        eprintln!("Error parsing arguments: {}", err);
        process::exit(1)
    });

    youtube_bz::run(&config.mbid).await;
}
