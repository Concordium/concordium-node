use crate::common::{ P2PPeer, P2PNodeId, get_current_stamp };
use crate::network::{
    PROTOCOL_NAME, PROTOCOL_VERSION, PROTOCOL_MESSAGE_TYPE_REQUEST_PING, 
    PROTOCOL_MESSAGE_TYPE_REQUEST_JOINNETWORK, PROTOCOL_MESSAGE_TYPE_REQUEST_LEAVENETWORK,
    PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE, PROTOCOL_MESSAGE_TYPE_REQUEST_BANNODE,
    PROTOCOL_MESSAGE_TYPE_REQUEST_UNBANNODE, PROTOCOL_MESSAGE_TYPE_REQUEST_HANDSHAKE,
    PROTOCOL_MESSAGE_TYPE_REQUEST_GET_PEERS,
};

#[derive(Debug, Clone)]
pub enum NetworkRequest {
    Ping(P2PPeer),
    FindNode(P2PPeer, P2PNodeId),
    BanNode(P2PPeer, P2PPeer),
    Handshake(P2PPeer, Vec<u16>, Vec<u8>),
    GetPeers(P2PPeer, Vec<u16>),
    UnbanNode(P2PPeer, P2PPeer),
    JoinNetwork(P2PPeer, u16),
    LeaveNetwork(P2PPeer, u16),
}

impl NetworkRequest {
    pub fn serialize(&self) -> Vec<u8> {
        match self {
            NetworkRequest::Ping(_) => {
                format!("{}{}{:016x}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_REQUEST_PING).as_bytes()
                                                           .to_vec()
            }
            NetworkRequest::JoinNetwork(_, nid) => {
                format!("{}{}{:016x}{}{:05}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_REQUEST_JOINNETWORK,
                        nid).as_bytes()
                            .to_vec()
            }
            NetworkRequest::LeaveNetwork(_, nid) => {
                format!("{}{}{:016x}{}{:05}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_REQUEST_LEAVENETWORK,
                        nid).as_bytes()
                            .to_vec()
            }
            NetworkRequest::FindNode(_, id) => {
                format!("{}{}{:016x}{}{:064x}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE,
                        id.get_id()).as_bytes()
                                    .to_vec()
            }
            NetworkRequest::BanNode(_, node_data) => {
                format!("{}{}{:016x}{}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_REQUEST_BANNODE,
                        node_data.serialize()).as_bytes()
                                              .to_vec()
            }
            NetworkRequest::UnbanNode(_, node_data) => {
                format!("{}{}{:016x}{}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_REQUEST_UNBANNODE,
                        node_data.serialize()).as_bytes()
                                              .to_vec()
            }
            NetworkRequest::Handshake(me, nids, zk) => {
                let mut pkt: Vec<u8> = Vec::new();
                for byte in format!("{}{}{:016x}{}{}{:05}{:05}{}{:010}",
                                    PROTOCOL_NAME,
                                    PROTOCOL_VERSION,
                                    get_current_stamp(),
                                    PROTOCOL_MESSAGE_TYPE_REQUEST_HANDSHAKE,
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
            NetworkRequest::GetPeers(_, networks) => {
                format!("{}{}{:016x}{}{:05}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_REQUEST_GET_PEERS,
                        networks.len(),
                        networks.iter()
                                .map(|x| format!("{:05}", x))
                                .collect::<String>()).as_bytes()
                                                     .to_vec()
            }
        }
    }
}


