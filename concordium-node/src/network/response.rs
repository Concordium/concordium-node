use common::{ P2PPeer, get_current_stamp };
use network::{
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
                        PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG).as_bytes()
                                                            .to_vec()
            }
            NetworkResponse::FindNode(_, peers) => {
                let mut buffer = String::new();
                for peer in peers.iter() {
                    buffer.push_str(&peer.serialize()[..]);
                }
                format!("{}{}{:016x}{}{:03}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_RESPONSE_FINDNODE,
                        peers.len(),
                        buffer).as_bytes()
                               .to_vec()
            }
            NetworkResponse::PeerList(_, peers) => {
                let mut buffer = String::new();
                for peer in peers.iter() {
                    buffer.push_str(&peer.serialize()[..]);
                }
                format!("{}{}{:016x}{}{:03}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_RESPONSE_PEERSLIST,
                        peers.len(),
                        buffer).as_bytes()
                               .to_vec()
            }
            NetworkResponse::Handshake(me, nids, zk) => {
                let mut pkt: Vec<u8> = Vec::new();
                for byte in format!("{}{}{:016x}{}{}{:05}{:05}{}{:010}",
                                    PROTOCOL_NAME,
                                    PROTOCOL_VERSION,
                                    get_current_stamp(),
                                    PROTOCOL_MESSAGE_TYPE_RESPONSE_HANDSHAKE,
                                    me.id().to_string(),
                                    me.port(),
                                    nids.len(),
                                    nids.iter().map(|x| format!("{:05}", x)).collect::<String>(),
                                    zk.len()).as_bytes()
                {
                    pkt.push(*byte);
                }
                for byte in zk.iter() {
                    pkt.push(*byte);
                }
                pkt
            }
        }
    }
}

