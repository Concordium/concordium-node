use common::{ P2PPeer, P2PNodeId, get_current_stamp };
use network:: {
    PROTOCOL_NAME, PROTOCOL_VERSION, PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE, 
    PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE
};

use utils;
use std::sync::{ Mutex };
use rand::rngs::OsRng;
use rand::{ RngCore };

lazy_static! {
    static ref RNG: Mutex<OsRng> = { Mutex::new(OsRng::new().unwrap()) };
}

#[derive(Debug, Clone)]
pub enum NetworkPacket {
    DirectMessage(P2PPeer, String, P2PNodeId, u16, Vec<u8>),
    BroadcastedMessage(P2PPeer, String, u16, Vec<u8>),
}

impl NetworkPacket {
    pub fn serialize(&self) -> Vec<u8> {
        match self {
            NetworkPacket::DirectMessage(_, msgid, receiver, nid, msg) => {
                let mut pkt: Vec<u8> = Vec::new();
                for byte in format!("{}{}{:016x}{}{:x}{}{:05}{:010}",
                                    PROTOCOL_NAME,
                                    PROTOCOL_VERSION,
                                    get_current_stamp(),
                                    PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE,
                                    receiver.get_id(),
                                    msgid,
                                    nid,
                                    msg.len()).as_bytes()
                {
                    pkt.push(*byte);
                }
                for byte in msg.iter() {
                    pkt.push(*byte);
                }
                pkt
            }
            NetworkPacket::BroadcastedMessage(_, msgid, nid, msg) => {
                let mut pkt: Vec<u8> = Vec::new();
                for byte in format!("{}{}{:016x}{}{}{:05}{:010}",
                                    PROTOCOL_NAME,
                                    PROTOCOL_VERSION,
                                    get_current_stamp(),
                                    PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE,
                                    msgid,
                                    nid,
                                    msg.len()).as_bytes()
                {
                    pkt.push(*byte);
                }
                for byte in msg.iter() {
                    pkt.push(*byte);
                }
                pkt
            }
        }
    }

    pub fn generate_message_id() -> String {
        let mut secure_bytes = vec![0u8; 256];
        RNG.lock().unwrap().fill_bytes(&mut secure_bytes);
        utils::to_hex_string(&utils::sha256_bytes(&secure_bytes))
    }
}


