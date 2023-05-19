//! BitTorent Protocol Implementation
//! BitTorrent is a protocol for distributing files.
//! It identifies content by URL and is designed to integrate
//! seamlessly with the web.
//! Its advantage over plain HTTP is that when multiple downloads
//! of the same file happen concurrently,
//! the downloaders upload to each other,
//! making it possible for the file source to support very
//! large numbers of downloaders with only a modest increase in its load.
//!
//! <https://www.bittorrent.org/beps/bep_0003.html>

use sha1::{Digest, Sha1};
use std::io::prelude::*;
use std::net::{SocketAddrV4, TcpStream};
pub mod handshake;
pub mod message;
pub mod metainfo;
mod tracker;
pub mod utils;

use crate::handshake::Handshake;
use crate::message::Message;
use crate::metainfo::Metainfo;

pub struct Peer {
    _socket_addr_v4: SocketAddrV4,
    metainfo: Metainfo,
    stream: TcpStream,
}

impl Peer {
    pub fn establish_connection(
        socket_addr_v4: SocketAddrV4,
        metainfo: Metainfo,
    ) -> Result<Peer, String> {
        Ok(Peer {
            _socket_addr_v4: socket_addr_v4,
            metainfo,
            stream: TcpStream::connect(socket_addr_v4).map_err(|_| "Couldn't connect to peer")?,
        })
    }

    pub fn establish_handshake(&mut self, peer_id: &str) {
        // Send handhake message
        self.stream
            .write(&Handshake::new(self.metainfo.get_info_hash(), peer_id).serialize())
            .unwrap();

        // Receive handshake response
        let _received_hanshake = Handshake::from_stream(self.stream.try_clone().unwrap());

        // Receive bitfield
        let _bitfield_message = Message::from_stream(self.stream.try_clone().unwrap());

        // Receive unchocke
        let _unchoke = Message::from_stream(self.stream.try_clone().unwrap());
    }

    pub async fn download_piece(&mut self, index: usize) {
        let mut buff: Vec<u8> = Vec::new();
        let mut offset = 0;
        let hash_list: Vec<&[u8]> = self.metainfo.info.pieces.chunks(20).collect();

        self.stream
            .write(&Message::interested().serialize())
            .unwrap();

        let _unchoke = Message::from_stream(self.stream.try_clone().unwrap());

        while offset < self.metainfo.info.piece_length {
            let length = 2_u32.pow(14);
            // Send request
            self.stream
                .write(&Message::request(index as u32, offset, length).serialize())
                .unwrap();

            // Receive piece
            let piece = Message::from_stream(self.stream.try_clone().unwrap());
            let p_index = u32::from_be_bytes(piece.payload.get(0..4).unwrap().try_into().unwrap());
            let p_offset = u32::from_be_bytes(piece.payload.get(4..8).unwrap().try_into().unwrap());
            let p_data = piece.payload.get(8..).unwrap();
            buff.append(&mut p_data.to_vec());
            offset = offset + length;
            println!("Received piece {} at offset {}", p_index, p_offset);
        }
        // Verify the piece checksum
        if !is_checksum_correct(buff, hash_list[index]) {
            println!("INVALID CHEKSUM")
        }
        // Send we have the piece
        self.stream
            .write(&Message::have(index as u32).serialize())
            .unwrap();
    }
}

fn is_checksum_correct(buff: Vec<u8>, hash: &[u8]) -> bool {
    let mut hasher = Sha1::new();
    hasher.update(&buff);
    let result = hasher.finalize();
    result[..] == hash[..]
}

fn _has_piece(bf: &Vec<u8>, index: u32) -> bool {
    let bytes_index = index / 8;
    let offset = index % 8;
    let val = bf[bytes_index as usize] >> (7 - offset) & 1;
    val != 0
}

#[test]
fn test_parsing_metainfo() {
    let data =
        std::fs::read("debian-11.5.0-amd64-netinst.iso.torrent").expect("Unable to read file");
    let deserialized = Metainfo::from_bytes(&data);
    assert_eq!(
        deserialized.announce,
        "http://bttracker.debian.org:6969/announce"
    );
    assert_eq!(deserialized.info.name, "debian-11.5.0-amd64-netinst.iso");
    assert_eq!(deserialized.info.piece_length, 262144);
    assert_eq!(deserialized.info.length, 400556032);
}

#[test]
fn test_get_peers() {
    let data =
        std::fs::read("debian-11.5.0-amd64-netinst.iso.torrent").expect("Unable to read file");
    let deserialized = Metainfo::from_bytes(&data);
    let _peers = deserialized.get_peers();
}

#[tokio::test]
async fn test_connecting_to_peers() {
    let data =
        std::fs::read("debian-11.5.0-amd64-netinst.iso.torrent").expect("Unable to read file");
    let peer_id = "-DE203s-x49Ta1Q*sgGQ";
    let peers = [SocketAddrV4::new(
        std::net::Ipv4Addr::new(127, 0, 0, 1),
        53709,
    )];


    for peer in peers {
        let metainfo = Metainfo::from_bytes(&data);
        let n_piece = metainfo.info.pieces.chunks(20).len();
        let handle = tokio::spawn(async move {
            let mut peer = Peer::establish_connection(peer, metainfo.clone()).unwrap();
            peer.establish_handshake(peer_id);
            for piece in 0..n_piece {
                peer.download_piece(piece).await;
            }
        });
        handle.await.unwrap();
    }
}
