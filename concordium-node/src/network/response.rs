use crate::common::{ P2PPeer, get_current_stamp };
use crate::network::{
    PROTOCOL_NAME, PROTOCOL_VERSION, PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG, 
    PROTOCOL_MESSAGE_TYPE_RESPONSE_FINDNODE, PROTOCOL_MESSAGE_TYPE_RESPONSE_HANDSHAKE,
    PROTOCOL_MESSAGE_TYPE_RESPONSE_PEERSLIST
};

#[derive(Debug, Clone)]
pub enum NetworkResponse {
    Pong(P2PPeer),
    FindNode(P2PPeer, Vec<P2PPeer>),
    PeerList(P2PPeer, Vec<P2PPeer>),
    Handshake(P2PPeer, Vec<u16>, Vec<u8>),
}

impl NetworkResponse {
    pub fn serialize(&self) -> Vec<u8> {
        match self {
            NetworkResponse::Pong(_) => {
                format!("{}{}{:016x}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG).into_bytes()
            }
            NetworkResponse::FindNode(_, peers) => {
                format!("{}{}{:016x}{}{:03}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_RESPONSE_FINDNODE,
                        peers.len(),
                        peers.iter().map(|peer| peer.serialize()).collect::<String>()
                ).into_bytes()
            }
            NetworkResponse::PeerList(_, peers) => {
                format!("{}{}{:016x}{}{:03}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_RESPONSE_PEERSLIST,
                        peers.len(),
                        peers.iter().map(|peer| peer.serialize()).collect::<String>()
                ).into_bytes()
            }
            NetworkResponse::Handshake(me, nids, zk) => {
                let mut pkt = format!("{}{}{:016x}{}{}{:05}{:05}{}{:010}",
                    PROTOCOL_NAME,
                    PROTOCOL_VERSION,
                    get_current_stamp(),
                    PROTOCOL_MESSAGE_TYPE_RESPONSE_HANDSHAKE,
                    me.id().to_string(),
                    me.port(),
                    nids.len(),
                    nids.iter().map(|x| format!("{:05}", x)).collect::<String>(),
                    zk.len()
                ).into_bytes();
                pkt.extend(zk.iter());

                pkt
            }
        }
    }
}

