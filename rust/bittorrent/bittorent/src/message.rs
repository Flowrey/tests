use std::io::prelude::*;
use std::net::TcpStream;
use std::vec;

/// Peer messages type
#[derive(Debug, Copy, Clone)]
pub enum MessageType {
    Chocke = 0,
    Unchoke = 1,
    Interested = 2,
    NotInterested = 3,
    Have = 4,
    Bitfield = 5,
    Request = 6,
    Piece = 7,
    Cancel = 8,
}

impl MessageType {
    pub fn from_u8(value: u8) -> Self {
        match value {
            0 => Self::Chocke,
            1 => Self::Unchoke,
            2 => Self::Interested,
            3 => Self::NotInterested,
            4 => Self::Have,
            5 => Self::Bitfield,
            6 => Self::Request,
            7 => Self::Piece,
            8 => Self::Cancel,
            _ => panic!("Unknown message type: {}", value),
        }
    }
}

/// Peer messages
#[derive(Debug)]
pub struct Message {
    pub length: u32,
    pub id: MessageType,
    pub payload: Vec<u8>,
}

impl Message {
    pub fn have(index: u32) -> Self {
        Self {
            length: 5,
            id: MessageType::Have,
            payload: index.to_be_bytes().to_vec(),
        }
    }

    pub fn request(index: u32, begin: u32, length: u32) -> Self {
        let mut payload = Vec::new();
        payload.append(&mut index.to_be_bytes().to_vec());
        payload.append(&mut begin.to_be_bytes().to_vec());
        payload.append(&mut length.to_be_bytes().to_vec());
        Self {
            length: 13,
            id: MessageType::Request,
            payload,
        }
    }

    pub fn interested() -> Self {
        Self {
            length: 1,
            id: MessageType::Interested,
            payload: vec![],
        }
    }

    pub fn serialize(&self) -> Vec<u8> {
        let mut buff = Vec::with_capacity(self.length as usize + 4);
        let length: [u8; 4] = self.length.to_be_bytes();
        buff.extend_from_slice(&length);
        buff.push(self.id as u8);
        buff.extend_from_slice(&self.payload);
        buff
    }

    pub fn from_stream(mut stream: TcpStream) -> Self {
        let mut length_buff = [0u8; 4];
        stream.read_exact(&mut length_buff).unwrap();

        let length = u32::from_be_bytes(length_buff);

        let mut buf = vec![0u8; length as usize];
        stream.read_exact(&mut buf).unwrap();

        let id = MessageType::from_u8(buf.remove(0));

        Message {
            length,
            id,
            payload: buf,
        }
    }
}
