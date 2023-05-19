use std::{net::TcpStream, io::Read};

pub struct Handshake {
    length: u8,
    pstr: [u8; 19],
    extensions: [u8; 8],
    info_hash: [u8; 20],
    peer_id: [u8; 20],
}

impl Handshake {
    pub fn new(info_hash: [u8; 20], peer_id: &str) -> Self {
        Handshake {
            length: 19,
            pstr: "BitTorrent protocol".as_bytes().try_into().unwrap(),
            extensions: [0u8; 8],
            info_hash,
            peer_id: peer_id.as_bytes().try_into().unwrap(),
        }
    }

    pub fn serialize(&self) -> [u8; 68] {
        let mut buff = [0u8; 68];
        buff[0] = self.length;
        buff[1..20].copy_from_slice(&self.pstr);
        buff[20..28].copy_from_slice(&self.extensions);
        buff[28..48].copy_from_slice(&self.info_hash);
        buff[48..68].copy_from_slice(&self.peer_id);
        buff
    }

    pub fn from_stream(mut stream: TcpStream) -> Self {
        let mut handshake_buffer = [0u8; 68];
        stream.read_exact(&mut handshake_buffer).unwrap();
        Self::deserialize(handshake_buffer)
    }

    pub fn deserialize(bytes: [u8; 68]) -> Self {
        Self {
            length: bytes[0],
            pstr: bytes[1..20].try_into().unwrap(),
            extensions: bytes[20..28].try_into().unwrap(),
            info_hash: bytes[28..48].try_into().unwrap(),
            peer_id: bytes[48..68].try_into().unwrap(),
        }
    }
}
