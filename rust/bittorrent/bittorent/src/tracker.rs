use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize)]
pub struct Request<'a> {
    // 20 bytes sha1 hash of the bencoded from of the
    // info value from the metainfo file.
    // info_hash: &'a str,

    /// A string of length 20 wich this downloader used as its id.
    pub peer_id: &'a str,

    // An optional parameter giving the IP which this peer is at.
    // ip: &'a str,

    // The port number this peer is listening on.
    pub port: u16,

    // The total amount uploaded so far, encoded in base ten ascii
    pub uploaded: u32,

    // The total amount downloaded so far
    pub downloaded: u32,

    // The number of bytes this peer still has to download,
    // encoded in base ten ascii.
    pub left: u32,

    // This is an optional key which maps to
    // started, completed, or stopped
    // event: &'a str,
    pub compact: u8,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Response<'a> {
    interval: u32,

    // Compact = 0
    // peers: Vec<Peer>,

    // Compact = 1
    #[serde(with = "serde_bytes")]
    pub peers: &'a [u8],
}
