//! MetaInfo module
use crate::utils::urlencode;
use sha1::{Digest, Sha1};
use url::Url;
use std::net::{Ipv4Addr, SocketAddrV4};
use serde::{Deserialize, Serialize};
use crate::tracker;

/// Informations about the Torrent
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Info {
    /// Suggested name to save the file as.
    pub name: String,

    /// Number of bytes in each piece the file is split into.
    #[serde(rename = "piece length")]
    pub piece_length: u32,

    /// Length of the file in bytes.
    pub length: u32,

    /// String whose length is a multiple of 20.
    /// It is to be subdivided into strings of length 20
    /// each of wich is the SHA1 hash of the piece at
    /// the corresponding index.
    #[serde(with = "serde_bytes")]
    pub pieces: Vec<u8>,
}

/// Metainfo files (also known as .torrent files)
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Metainfo {
    /// The URL of the tracker.
    pub announce: String,

    /// This maps to a Info struct.
    pub info: Info,
}

impl Metainfo {
    pub fn from_bytes(bytes: &[u8]) -> Self {
        bendy::serde::from_bytes::<Metainfo>(&bytes).expect("Failed to deserialize torrent file")
    }

    pub fn get_info_hash(&self) -> [u8; 20] {
        let mut hasher = Sha1::new();
        let bencoded_info = bendy::serde::to_bytes(&self.info).expect("Failed to encode info");
        hasher.update(bencoded_info);
        hasher
            .finalize()
            .try_into()
            .expect("hash must be of size 20")
    }

    pub fn get_peers(&self) -> Vec<SocketAddrV4> {
        let info_hash = urlencode(&self.get_info_hash());

        let mut url = Url::parse(&self.announce).expect("Not a valid announce url");
        url.set_query(Some(&format!("info_hash={}", info_hash)));

        let payload = tracker::Request {
            peer_id: "-DE203s-x49Ta1Q*sgGQ",
            port: 58438,
            uploaded: 0,
            downloaded: 0,
            left: self.info.length,
            compact: 1,
        };

        let client = reqwest::blocking::Client::new();
        let res = match client.get(url).query(&payload).send() {
            Ok(r) => r.bytes(),
            Err(e) => panic!("Failed to establish connection to tracker: {}", e),
        };

        let input = res.unwrap();
        let de_res = match bendy::serde::from_bytes::<tracker::Response>(&input) {
            Ok(v) => v,
            Err(_) => panic!("Failed to deserialize tracker response"),
        };

        let chunked_peers = de_res.peers.chunks_exact(6);
        let mut peers: Vec<SocketAddrV4> = Vec::new();
        for peer in chunked_peers {
            let ip: [u8; 4] = peer[..4].try_into().unwrap();
            let ip = Ipv4Addr::from(ip);

            let port: [u8; 2] = peer[4..6].try_into().unwrap();
            let port = u16::from_be_bytes(port);

            let socket = SocketAddrV4::new(ip, port);
            peers.push(socket);
        }
        peers
    }
}
