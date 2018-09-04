use num_bigint::BigUint;
use num_traits::Num;
use std::cmp::Ordering;
use std::net::IpAddr;
use std::str;
use std::str::FromStr;
use time;
use utils;

pub const PROTOCOL_NAME: &'static str = "CONCORDIUMP2P";
pub const PROTOCOL_VERSION: &'static str = "001";
pub const PROTOCOL_NODE_ID_LENGTH: usize = 64;
pub const PROTOCOL_SENT_TIMESTAMP_LENGTH: usize = 16;

pub const PROTOCOL_MESSAGE_TYPE_REQUEST_PING: &'static str = "0001";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE: &'static str = "0002";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_HANDSHAKE: &'static str = "0003";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_GET_PEERS: &'static str = "0004";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_BANNODE: &'static str = "0005";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_UNBANNODE: &'static str = "0006";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_JOINNETWORK: &'static str = "0007";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_LEAVENETWORK: &'static str = "0008";
pub const PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG: &'static str = "1001";
pub const PROTOCOL_MESSAGE_TYPE_RESPONSE_FINDNODE: &'static str = "1002";
pub const PROTOCOL_MESSAGE_TYPE_RESPONSE_PEERSLIST: &'static str = "1003";
pub const PROTOCOL_MESSAGE_TYPE_RESPONSE_HANDSHAKE: &'static str = "1004";
pub const PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE: &'static str = "2001";
pub const PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE: &'static str = "2002";

#[derive(Debug, Clone)]
pub enum NetworkMessage {
    NetworkRequest(NetworkRequest, Option<u64>, Option<u64>),
    NetworkResponse(NetworkResponse, Option<u64>, Option<u64>),
    NetworkPacket(NetworkPacket, Option<u64>, Option<u64>),
    UnknownMessage,
    InvalidMessage,
}

impl NetworkMessage {
    pub fn deserialize(bytes: &[u8]) -> NetworkMessage {
        let protocol_name_length = PROTOCOL_NAME.len();
        let protocol_version_length = PROTOCOL_VERSION.len();
        if bytes.len() >= protocol_name_length
           && str::from_utf8(&bytes[..protocol_name_length]).unwrap() == PROTOCOL_NAME
        {
            if bytes.len() >= protocol_name_length + protocol_version_length && str::from_utf8(&bytes[protocol_name_length..(protocol_name_length + protocol_version_length)]).unwrap() == PROTOCOL_VERSION {
                if bytes.len() < protocol_name_length + protocol_version_length + 4 + PROTOCOL_SENT_TIMESTAMP_LENGTH {
                    return NetworkMessage::InvalidMessage;
                }
                let timestamp_bytes = &bytes[(protocol_name_length + protocol_version_length)..(protocol_name_length + protocol_version_length + PROTOCOL_SENT_TIMESTAMP_LENGTH)];
                let timestamp = match u64::from_str_radix(str::from_utf8(&timestamp_bytes).unwrap(), 16) {
                    Ok(n) => n,
                    _ => return NetworkMessage::InvalidMessage,
                };
                let header = protocol_name_length + protocol_version_length + PROTOCOL_SENT_TIMESTAMP_LENGTH;
                let message_type_id = str::from_utf8(&bytes[header..(header + 4)]).unwrap();
                let inner_msg_size = header + 4;
                match message_type_id as &str {
                    PROTOCOL_MESSAGE_TYPE_REQUEST_PING => {
                        let sender = P2PPeer::deserialize(str::from_utf8(&bytes[inner_msg_size..]).unwrap());
                        match sender {
                            Some(peer) => NetworkMessage::NetworkRequest(NetworkRequest::Ping(peer), Some(timestamp), Some(get_current_stamp())),
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG => {
                        let sender = P2PPeer::deserialize(str::from_utf8(&bytes[inner_msg_size..]).unwrap());
                        match sender {
                            Some(peer) => NetworkMessage::NetworkResponse(NetworkResponse::Pong(peer), Some(timestamp), Some(get_current_stamp())),
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_RESPONSE_HANDSHAKE => {
                        let sender = P2PPeer::deserialize(str::from_utf8(&bytes[inner_msg_size..]).unwrap());
                        match sender {
                            Some(peer) => {
                                let sender_size = peer.serialize().len();
                                if bytes.len() >= sender_size+inner_msg_size+5 {
                                    match str::from_utf8(&bytes[(inner_msg_size+sender_size)..(5 + sender_size+inner_msg_size)]).unwrap().parse::<usize>() {
                                        Ok(nids) => {
                                            if bytes.len() >= sender_size+inner_msg_size+5+(nids*5) {
                                                let mut loaded_nids: Vec<u8> = vec![];
                                                for nid_id in 0..nids {
                                                    match str::from_utf8(&bytes[(inner_msg_size+sender_size+(nid_id*5)+5)..(10 + sender_size+inner_msg_size+(nid_id*5))]).unwrap().parse::<u8>() {
                                                        Ok(loaded_nid) => loaded_nids.push(loaded_nid),
                                                        _ => error!("Can't load one of the network ids given")
                                                    }
                                                }
                                                if bytes.len() >= loaded_nids.len()*5+5+sender_size+inner_msg_size+10 {
                                                    match str::from_utf8(&bytes[(loaded_nids.len()*5+5+sender_size+inner_msg_size)..loaded_nids.len()*5+5+sender_size+inner_msg_size+10]).unwrap().parse::<usize>() {
                                                        Ok(csize) => {
                                                            if bytes.len() == loaded_nids.len()*5+5+sender_size+inner_msg_size+csize+10 {
                                                                NetworkMessage::NetworkResponse(NetworkResponse::Handshake(peer, loaded_nids, bytes[(10 + sender_size+inner_msg_size+(nids*5)+5)..].to_vec()), Some(timestamp), Some(get_current_stamp()))
                                                            } else {
                                                                NetworkMessage::InvalidMessage
                                                            }
                                                        }
                                                        _ => NetworkMessage::InvalidMessage
                                                    }
                                                } else {
                                                    NetworkMessage::InvalidMessage
                                                }
                                            } else {
                                                NetworkMessage::InvalidMessage
                                            }
                                        }
                                        _ => NetworkMessage::InvalidMessage
                                    }
                                } else {
                                    NetworkMessage::InvalidMessage
                                }
                            }
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_REQUEST_GET_PEERS => {
                        let sender = P2PPeer::deserialize(str::from_utf8(&bytes[inner_msg_size..]).unwrap());
                        match sender {
                            Some(peer) => NetworkMessage::NetworkRequest(NetworkRequest::GetPeers(peer), Some(timestamp), Some(get_current_stamp())),
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_REQUEST_HANDSHAKE => {
                        let sender = P2PPeer::deserialize(str::from_utf8(&bytes[inner_msg_size..]).unwrap());
                        match sender {
                            Some(peer) => {
                                let sender_size = peer.serialize().len();
                                if bytes.len() >= sender_size+inner_msg_size+5 {
                                    match str::from_utf8(&bytes[(inner_msg_size+sender_size)..(5 + sender_size+inner_msg_size)]).unwrap().parse::<usize>() {
                                        Ok(nids) => {
                                            if bytes.len() >= sender_size+inner_msg_size+5+(nids*5) {
                                                let mut loaded_nids: Vec<u8> = vec![];
                                                for nid_id in 0..nids {
                                                    match str::from_utf8(&bytes[(inner_msg_size+sender_size+(nid_id*5)+5)..(10 + sender_size+inner_msg_size+(nid_id*5))]).unwrap().parse::<u8>() {
                                                        Ok(loaded_nid) => loaded_nids.push(loaded_nid),
                                                        _ => error!("Can't load one of the network ids given")
                                                    }
                                                }
                                                if bytes.len() >= loaded_nids.len()*5+5+sender_size+inner_msg_size+10 {
                                                    match str::from_utf8(&bytes[(loaded_nids.len()*5+5+sender_size+inner_msg_size)..loaded_nids.len()*5+5+sender_size+inner_msg_size+10]).unwrap().parse::<usize>() {
                                                        Ok(csize) => {
                                                            if bytes.len() == loaded_nids.len()*5+5+sender_size+inner_msg_size+csize+10 {
                                                                NetworkMessage::NetworkRequest(NetworkRequest::Handshake(peer, loaded_nids, bytes[(10 + sender_size+inner_msg_size+(nids*5)+5)..].to_vec()), Some(timestamp), Some(get_current_stamp()))
                                                            } else {
                                                                NetworkMessage::InvalidMessage
                                                            }
                                                        }
                                                        _ => NetworkMessage::InvalidMessage
                                                    }
                                                } else {
                                                    NetworkMessage::InvalidMessage
                                                }
                                            } else {
                                                NetworkMessage::InvalidMessage
                                            }
                                        }
                                        _ => NetworkMessage::InvalidMessage
                                    }
                                } else {
                                    NetworkMessage::InvalidMessage
                                }
                            }
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE => {
                        let sender = P2PPeer::deserialize(str::from_utf8(&bytes[inner_msg_size..]).unwrap());
                        match sender {
                            Some(sender) => {
                                let sender_len = sender.serialize().len();
                                if bytes.len() != inner_msg_size + sender_len + PROTOCOL_NODE_ID_LENGTH {
                                    return NetworkMessage::InvalidMessage;
                                }
                                let node_id = str::from_utf8(&bytes[(inner_msg_size + sender_len)..]).unwrap();

                                NetworkMessage::NetworkRequest(NetworkRequest::FindNode(sender, P2PNodeId::from_string(&node_id.to_string()).unwrap()), Some(timestamp), Some(get_current_stamp()))
                            }
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_REQUEST_BANNODE => {
                        let sender = P2PPeer::deserialize(str::from_utf8(&bytes[inner_msg_size..]).unwrap());
                        match sender {
                            Some(sender) => {
                                let sender_len = sender.serialize().len();
                                let node_data = P2PPeer::deserialize(str::from_utf8(&bytes[(inner_msg_size + sender_len)..]).unwrap());
                                match node_data {
                                    Some(node_info) => NetworkMessage::NetworkRequest(NetworkRequest::BanNode(sender, node_info), Some(timestamp), Some(get_current_stamp())),
                                    _ => NetworkMessage::InvalidMessage,
                                }
                            }
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_REQUEST_UNBANNODE => {
                        let sender = P2PPeer::deserialize(str::from_utf8(&bytes[inner_msg_size..]).unwrap());
                        match sender {
                            Some(sender) => {
                                let sender_len = sender.serialize().len();
                                let node_data = P2PPeer::deserialize(str::from_utf8(&bytes[(inner_msg_size + sender_len)..]).unwrap());
                                match node_data {
                                    Some(node_info) => NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(sender, node_info), Some(timestamp), Some(get_current_stamp())),
                                    _ => NetworkMessage::InvalidMessage,
                                }
                            }
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_REQUEST_JOINNETWORK => {
                        let sender = P2PPeer::deserialize(str::from_utf8(&bytes[inner_msg_size..]).unwrap());
                        match sender {
                            Some(sender) => {
                                let sender_len = sender.serialize().len();
                                if bytes[(inner_msg_size+sender_len)..].len() == 5 {
                                    match str::from_utf8(&bytes[(inner_msg_size+sender_len)..]).unwrap().parse::<u8>() {
                                        Ok(network_id) => NetworkMessage::NetworkRequest(NetworkRequest::JoinNetwork(sender, network_id), Some(timestamp), Some(get_current_stamp())),
                                        _ => NetworkMessage::InvalidMessage,
                                    }
                                } else {
                                    NetworkMessage::InvalidMessage
                                } 
                            }
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_REQUEST_LEAVENETWORK => {
                        let sender = P2PPeer::deserialize(str::from_utf8(&bytes[inner_msg_size..]).unwrap());
                        match sender {
                            Some(sender) => {
                                let sender_len = sender.serialize().len();
                                if bytes[(inner_msg_size+sender_len)..].len() == 5 {
                                    match str::from_utf8(&bytes[(inner_msg_size+sender_len)..]).unwrap().parse::<u8>() {
                                        Ok(network_id) => NetworkMessage::NetworkRequest(NetworkRequest::LeaveNetwork(sender, network_id), Some(timestamp), Some(get_current_stamp())),
                                        _ => NetworkMessage::InvalidMessage,
                                    }
                                } else {
                                    NetworkMessage::InvalidMessage
                                } 
                            }
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_RESPONSE_FINDNODE => {
                        let inner_msg = &bytes[inner_msg_size..];
                        let sender = P2PPeer::deserialize(str::from_utf8(&bytes[inner_msg_size..]).unwrap());
                        match sender {
                            Some(sender) => {
                                let sender_len = sender.serialize().len();
                                if inner_msg.len() < (3 + sender_len) {
                                    return NetworkMessage::InvalidMessage;
                                }
                                let peers_count: Option<usize> = match str::from_utf8(&inner_msg[sender_len..(3 + sender_len)]).unwrap().parse::<usize>() {
                                    Ok(n) => Some(n),
                                    Err(_) => None,
                                };
                                if peers_count.is_none() || peers_count.unwrap() == 0 {
                                    return NetworkMessage::NetworkResponse(NetworkResponse::FindNode(sender, vec![]), Some(timestamp), Some(get_current_stamp()));
                                }

                                let mut current_peer_start: usize = 3 + sender_len;
                                let mut peers: Vec<P2PPeer> = vec![];
                                for _ in 0..peers_count.unwrap() {
                                    match P2PPeer::deserialize(str::from_utf8(&inner_msg[current_peer_start..]).unwrap()) {
                                        Some(peer) => {
                                            current_peer_start += &peer.serialize().len();
                                            peers.push(peer);
                                        }
                                        _ => return NetworkMessage::InvalidMessage,
                                    }
                                }
                                return NetworkMessage::NetworkResponse(NetworkResponse::FindNode(sender, peers), Some(timestamp), Some(get_current_stamp()));
                            }
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_RESPONSE_PEERSLIST => {
                        let inner_msg = &bytes[inner_msg_size..];
                        let sender = P2PPeer::deserialize(str::from_utf8(&bytes[inner_msg_size..]).unwrap());
                        match sender {
                            Some(sender) => {
                                let sender_len = sender.serialize().len();
                                if inner_msg.len() < (3 + sender_len) {
                                    return NetworkMessage::InvalidMessage;
                                }
                                let peers_count: Option<usize> = match str::from_utf8(&inner_msg[sender_len..(3 + sender_len)]).unwrap().parse::<usize>() {
                                    Ok(n) => Some(n),
                                    Err(_) => None,
                                };
                                if peers_count.is_none() || peers_count.unwrap() == 0 {
                                    return NetworkMessage::NetworkResponse(NetworkResponse::PeerList(sender, vec![]), Some(timestamp), Some(get_current_stamp()));
                                }

                                let mut current_peer_start: usize = 3 + sender_len;
                                let mut peers: Vec<P2PPeer> = vec![];
                                for _ in 0..peers_count.unwrap() {
                                    match P2PPeer::deserialize(str::from_utf8(&inner_msg[current_peer_start..]).unwrap()) {
                                        Some(peer) => {
                                            current_peer_start += &peer.serialize().len();
                                            peers.push(peer);
                                        }
                                        _ => return NetworkMessage::InvalidMessage,
                                    }
                                }
                                return NetworkMessage::NetworkResponse(NetworkResponse::PeerList(sender, peers), Some(timestamp), Some(get_current_stamp()));
                            }
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE => {
                        let sender = P2PPeer::deserialize(str::from_utf8(&bytes[inner_msg_size..]).unwrap());
                        match sender {
                            Some(peer) => {
                                let sender_len = &peer.serialize().len();
                                if bytes[(inner_msg_size + sender_len)..].len() < (15 + PROTOCOL_NODE_ID_LENGTH) {
                                    return NetworkMessage::InvalidMessage;
                                }                                
                                let remainer = inner_msg_size + sender_len;
                                let receiver_id = P2PNodeId::from_string(&(str::from_utf8(&bytes[remainer..(remainer + PROTOCOL_NODE_ID_LENGTH)]).unwrap()).to_string()).unwrap();
                                let network_id = match str::from_utf8(&bytes[(remainer+PROTOCOL_NODE_ID_LENGTH)..(remainer+PROTOCOL_NODE_ID_LENGTH+5)]).unwrap().parse::<u8>() {
                                    Ok(nid) => nid,
                                    _ => 0,
                                };
                                match str::from_utf8(&bytes[(remainer + PROTOCOL_NODE_ID_LENGTH + 5)..(remainer + 15 + PROTOCOL_NODE_ID_LENGTH)]).unwrap().parse::<usize>() {
                                    Ok(csize) => {
                                        if bytes[(remainer + PROTOCOL_NODE_ID_LENGTH + 15)..].len() != csize {
                                            return NetworkMessage::InvalidMessage;
                                        }
                                        return NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(peer, receiver_id, network_id, bytes[(remainer + PROTOCOL_NODE_ID_LENGTH + 15)..(remainer + PROTOCOL_NODE_ID_LENGTH + 15 + csize)].to_vec()), Some(timestamp), Some(get_current_stamp()));
                                    }
                                    Err(_) => return NetworkMessage::InvalidMessage,
                                }
                            }
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE => {
                        if &bytes.len() < &(inner_msg_size + 10) {
                            return NetworkMessage::InvalidMessage;
                        }
                        let sender = P2PPeer::deserialize(str::from_utf8(&bytes[inner_msg_size..]).unwrap());
                        match sender {
                            Some(peer) => {
                                let sender_len = &peer.serialize().len();
                                if bytes[(inner_msg_size + sender_len)..].len() < 15 {
                                    return NetworkMessage::InvalidMessage;
                                }
                                let network_id = match str::from_utf8(&bytes[(inner_msg_size+sender_len)..(inner_msg_size+sender_len+5)]).unwrap().parse::<u8>() {
                                    Ok(nid) => nid,
                                    _ => 0,
                                };
                                match str::from_utf8(&bytes[(inner_msg_size + sender_len + 5)..(inner_msg_size + sender_len + 15)]).unwrap().parse::<usize>() {
                                    Ok(csize) => {
                                        if bytes[(inner_msg_size + sender_len + 15)..].len() != csize {
                                            return NetworkMessage::InvalidMessage;
                                        }
                                        return NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(peer, network_id, bytes[(sender_len + inner_msg_size + 15)..(inner_msg_size + 15 + csize + sender_len)].to_vec()), Some(timestamp), Some(get_current_stamp()));
                                    }
                                    Err(_) => return NetworkMessage::InvalidMessage,
                                }
                            }
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    _ => NetworkMessage::UnknownMessage,
                }
            } else {
                NetworkMessage::InvalidMessage
            }
        } else {
            NetworkMessage::InvalidMessage
        }
    }
}

#[derive(Debug, Clone)]
pub enum NetworkPacket {
    DirectMessage(P2PPeer, P2PNodeId, u8, Vec<u8>),
    BroadcastedMessage(P2PPeer, u8, Vec<u8>),
}

impl NetworkPacket {
    pub fn serialize(&self) -> Vec<u8> {
        match self {
            NetworkPacket::DirectMessage(me, receiver, nid, msg) => {
                let mut pkt: Vec<u8> = Vec::new();
                for byte in format!("{}{}{:016x}{}{}{:x}{:05}{:010}",
                                    PROTOCOL_NAME,
                                    PROTOCOL_VERSION,
                                    get_current_stamp(),
                                    PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE,
                                    me.serialize(),
                                    receiver.get_id(),
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
            NetworkPacket::BroadcastedMessage(me, nid, msg) => {
                let mut pkt: Vec<u8> = Vec::new();
                for byte in format!("{}{}{:016x}{}{}{:05}{:010}",
                                    PROTOCOL_NAME,
                                    PROTOCOL_VERSION,
                                    get_current_stamp(),
                                    PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE,
                                    me.serialize(),
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
}

#[derive(Debug, Clone)]
pub enum NetworkRequest {
    Ping(P2PPeer),
    FindNode(P2PPeer, P2PNodeId),
    BanNode(P2PPeer, P2PPeer),
    Handshake(P2PPeer, Vec<u8>, Vec<u8>),
    GetPeers(P2PPeer),
    UnbanNode(P2PPeer, P2PPeer),
    JoinNetwork(P2PPeer,u8),
    LeaveNetwork(P2PPeer,u8)
}

impl NetworkRequest {
    pub fn serialize(&self) -> Vec<u8> {
        match self {
            NetworkRequest::Ping(me) => {
                format!("{}{}{:016x}{}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_REQUEST_PING,
                        me.serialize()).as_bytes()
                                       .to_vec()
            }
            NetworkRequest::JoinNetwork(me,nid) => {
                format!("{}{}{:016x}{}{}{:05}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_REQUEST_JOINNETWORK,
                        me.serialize(),
                        nid).as_bytes()
                                       .to_vec()
            }
            NetworkRequest::LeaveNetwork(me,nid) => {
                format!("{}{}{:016x}{}{}{:05}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_REQUEST_LEAVENETWORK,
                        me.serialize(),
                        nid).as_bytes()
                                       .to_vec()
            }
            NetworkRequest::FindNode(me, id) => {
                format!("{}{}{:016x}{}{}{:064x}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE,
                        me.serialize(),
                        id.get_id()).as_bytes()
                                    .to_vec()
            }
            NetworkRequest::BanNode(me, node_data) => {
                format!("{}{}{:016x}{}{}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_REQUEST_BANNODE,
                        me.serialize(),
                        node_data.serialize()).as_bytes()
                                              .to_vec()
            }
            NetworkRequest::UnbanNode(me, node_data) => {
                format!("{}{}{:016x}{}{}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_REQUEST_UNBANNODE,
                        me.serialize(),
                        node_data.serialize()).as_bytes()
                                              .to_vec()
            }
            NetworkRequest::Handshake(me,nids,zk) => {
                let mut pkt: Vec<u8> = Vec::new();
                for byte in format!("{}{}{:016x}{}{}{:05}{}{:010}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_REQUEST_HANDSHAKE,
                        me.serialize(),
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
            NetworkRequest::GetPeers(me) => {
                format!("{}{}{:016x}{}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_REQUEST_GET_PEERS,
                        me.serialize()).as_bytes()
                                       .to_vec()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum NetworkResponse {
    Pong(P2PPeer),
    FindNode(P2PPeer, Vec<P2PPeer>),
    PeerList(P2PPeer, Vec<P2PPeer>),
    Handshake(P2PPeer, Vec<u8>, Vec<u8>),
}

impl NetworkResponse {
    pub fn serialize(&self) -> Vec<u8> {
        match self {
            NetworkResponse::Pong(me) => {
                format!("{}{}{:016x}{}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG,
                        me.serialize()).as_bytes()
                                       .to_vec()
            }
            NetworkResponse::FindNode(me, peers) => {
                let mut buffer = String::new();
                for peer in peers.iter() {
                    buffer.push_str(&peer.serialize()[..]);
                }
                format!("{}{}{:016x}{}{}{:03}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_RESPONSE_FINDNODE,
                        me.serialize(),
                        peers.len(),
                        buffer).as_bytes()
                               .to_vec()
            }
            NetworkResponse::PeerList(me, peers) => {
                let mut buffer = String::new();
                for peer in peers.iter() {
                    buffer.push_str(&peer.serialize()[..]);
                }
                format!("{}{}{:016x}{}{}{:03}{}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_RESPONSE_PEERSLIST,
                        me.serialize(),
                        peers.len(),
                        buffer).as_bytes()
                               .to_vec()
            }
            NetworkResponse::Handshake(me, nids, zk) => {
                let mut pkt: Vec<u8> = Vec::new();
                for byte in format!("{}{}{:016x}{}{}{:05}{}{:010}",
                        PROTOCOL_NAME,
                        PROTOCOL_VERSION,
                        get_current_stamp(),
                        PROTOCOL_MESSAGE_TYPE_RESPONSE_HANDSHAKE,
                        me.serialize(),
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

#[derive(Debug, Clone, Hash)]
pub struct P2PPeer {
    ip: IpAddr,
    port: u16,
    id: P2PNodeId,
    last_seen: u64,
}

impl P2PPeer {
    pub fn new(ip: IpAddr, port: u16) -> Self {
        P2PPeer { ip,
                  port,
                  id: P2PNodeId::from_ip_port(ip, port),
                  last_seen: get_current_stamp(), }
    }

    pub fn from(id: P2PNodeId, ip: IpAddr, port: u16) -> Self {
        P2PPeer { id,
                  ip,
                  port,
                  last_seen: get_current_stamp(), }
    }

    pub fn serialize(&self) -> String {
        match &self.ip {
            IpAddr::V4(ip4) => (format!("{:064x}IP4{:03}{:03}{:03}{:03}{:05}", self.id.get_id(), ip4.octets()[0], ip4.octets()[1], ip4.octets()[2], ip4.octets()[3], self.port)[..]).to_string(),
            IpAddr::V6(ip6) => (format!("{:064x}IP6{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:05}",
                                        self.id.get_id(),
                                        ip6.octets()[0],
                                        ip6.octets()[1],
                                        ip6.octets()[2],
                                        ip6.octets()[3],
                                        ip6.octets()[4],
                                        ip6.octets()[5],
                                        ip6.octets()[6],
                                        ip6.octets()[7],
                                        ip6.octets()[8],
                                        ip6.octets()[9],
                                        ip6.octets()[10],
                                        ip6.octets()[11],
                                        ip6.octets()[12],
                                        ip6.octets()[13],
                                        ip6.octets()[14],
                                        ip6.octets()[15],
                                        self.port)[..])
                                                       .to_string(),
        }
    }

    pub fn deserialize(buf: &str) -> Option<P2PPeer> {
        if &buf.len() > &(PROTOCOL_NODE_ID_LENGTH + 3) {
            let node_id =
                P2PNodeId::from_string(&buf[..PROTOCOL_NODE_ID_LENGTH].to_string()).unwrap();
            let ip_type = &buf[PROTOCOL_NODE_ID_LENGTH..(PROTOCOL_NODE_ID_LENGTH + 3)];
            let ip_start = PROTOCOL_NODE_ID_LENGTH + 3;
            match ip_type {
                "IP4" => {
                    if &buf.len() >= &(PROTOCOL_NODE_ID_LENGTH + 3 + 12 + 5) {
                        match IpAddr::from_str(&format!("{}.{}.{}.{}",
                                                        &buf[ip_start..(ip_start + 3)],
                                                        &buf[(ip_start + 3)..(ip_start + 6)],
                                                        &buf[(ip_start + 6)..(ip_start + 9)],
                                                        &buf[(ip_start + 9)..(ip_start + 12)])[..])
                        {
                            Ok(ip_addr) => {
                                match buf[(ip_start + 12)..(ip_start + 17)].parse::<u16>() {
                                    Ok(port) => {
                                        return Some(P2PPeer { id: node_id,
                                                              ip: ip_addr,
                                                              port: port,
                                                              last_seen: get_current_stamp(), })
                                    }
                                    Err(_) => return None,
                                }
                            }
                            Err(_) => return None,
                        };
                    } else {
                        return None;
                    }
                }
                "IP6" => {
                    if &buf.len() >= &(PROTOCOL_NODE_ID_LENGTH + 3 + 32 + 5) {
                        match IpAddr::from_str(&format!("{}:{}:{}:{}:{}:{}:{}:{}",
                                                        &buf[ip_start..(ip_start + 4)],
                                                        &buf[(ip_start + 4)..(ip_start + 8)],
                                                        &buf[(ip_start + 8)..(ip_start + 12)],
                                                        &buf[(ip_start + 12)..(ip_start + 16)],
                                                        &buf[(ip_start + 16)..(ip_start + 20)],
                                                        &buf[(ip_start + 20)..(ip_start + 24)],
                                                        &buf[(ip_start + 24)..(ip_start + 28)],
                                                        &buf[(ip_start + 28)..(ip_start + 32)])[..])
                        {
                            Ok(ip_addr) => {
                                match buf[(ip_start + 32)..(ip_start + 37)].parse::<u16>() {
                                    Ok(port) => {
                                        let _node_id = P2PNodeId::from_ip_port(ip_addr, port);
                                        if _node_id.get_id() == node_id.get_id() {
                                            return Some(P2PPeer { id: node_id,
                                                                  ip: ip_addr,
                                                                  port: port,
                                                                  last_seen: get_current_stamp(), });
                                        } else {
                                            return None;
                                        }
                                    }
                                    Err(_) => return None,
                                }
                            }
                            Err(_) => return None,
                        };
                    } else {
                        return None;
                    }
                }
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn id(&self) -> P2PNodeId {
        self.id.clone()
    }

    pub fn ip(&self) -> IpAddr {
        self.ip
    }

    pub fn port(&self) -> u16 {
        self.port
    }

    pub fn last_seen(&self) -> u64 {
        self.last_seen
    }
}

impl PartialEq for P2PPeer {
    fn eq(&self, other: &P2PPeer) -> bool {
        self.id.id == other.id().id
    }
}

impl Ord for P2PPeer {
    fn cmp(&self, other: &P2PPeer) -> Ordering {
        self.id.id.cmp(&other.id().id)
    }
}

impl PartialOrd for P2PPeer {
    fn partial_cmp(&self, other: &P2PPeer) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for P2PPeer {}

#[derive(Debug, Clone, Hash)]
pub struct P2PNodeId {
    id: BigUint,
}

impl PartialEq for P2PNodeId {
    fn eq(&self, other: &P2PNodeId) -> bool {
        self.id == other.id
    }
}

impl Eq for P2PNodeId {}

impl P2PNodeId {
    pub fn from_string(sid: &String) -> Result<P2PNodeId, String> {
        match BigUint::from_str_radix(&sid, 16) {
            Ok(x) => Ok(P2PNodeId { id: x }),
            Err(_) => Err(format!("Can't parse {} as base16 number", &sid)),
        }
    }

    pub fn get_id(&self) -> &BigUint {
        &self.id
    }

    pub fn to_string(self) -> String {
        format!("{:064x}", self.id)
    }

    pub fn from_ip_port(ip: IpAddr, port: u16) -> P2PNodeId {
        let ip_port = format!("{}:{}", ip, port);
        P2PNodeId::from_string(&utils::to_hex_string(&utils::sha256(&ip_port))).unwrap()
    }

    pub fn from_ipstring(ip_port: String) -> P2PNodeId {
        P2PNodeId::from_string(&utils::to_hex_string(&utils::sha256(&ip_port))).unwrap()
    }
}

pub fn get_current_stamp() -> u64 {
    let current_time = time::get_time();
    (current_time.sec as u64 * 1000) + (current_time.nsec as u64 / 1000 / 1000)
}

#[cfg(test)]
mod tests {
    use common::*;
    #[test]
    pub fn req_ping_test() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let test_msg = NetworkRequest::Ping(self_peer);
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::Ping(_), _, _) => true,
                    _ => false,
                })
    }

    #[test]
    pub fn resp_pong_test() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let test_msg = NetworkResponse::Pong(self_peer);
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::Pong(_), _, _) => true,
                    _ => false,
                })
    }

    #[test]
    pub fn resp_handshake() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let nets = vec![0, 100];
        let test_zk = String::from("Random zk data");
        let test_msg = NetworkResponse::Handshake(self_peer, nets.clone(), test_zk.as_bytes().to_vec());
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::Handshake(_, _nets,_zk), _, _) => _zk == test_zk.as_bytes().to_vec() && nets == _nets,
                    _ => false,
                })
    }

    #[test]
    pub fn req_handshake() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let nets = vec![0, 100];
        let test_zk = String::from("Random zk data");
        let test_msg = NetworkRequest::Handshake(self_peer, nets.clone(), test_zk.as_bytes().to_vec());
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::Handshake(_,_nets,_zk), _, _) => _zk == test_zk.as_bytes().to_vec() && nets == _nets,
                    _ => false,
                })
    }

    #[test]
    pub fn resp_handshake_nozk() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let nets = vec![0, 100];
        let test_msg = NetworkResponse::Handshake(self_peer, nets.clone(), vec![]);
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::Handshake(_, _nets,_zk), _, _) => _zk.len() == 0 && nets == _nets,
                    _ => false,
                })
    }

    #[test]
    pub fn req_handshake_nozk() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let nets = vec![0, 100];
        let test_msg = NetworkRequest::Handshake(self_peer, nets.clone(), vec![]);
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::Handshake(_,_nets,_zk), _, _) => _zk.len() == 0 && nets == _nets,
                    _ => false,
                })
    }

    #[test]
    pub fn resp_handshake_nozk_nonets() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let nets = vec![];
        let test_msg = NetworkResponse::Handshake(self_peer, nets.clone(), vec![]);
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::Handshake(_, _nets,_zk), _, _) => _zk.len() == 0 && _nets.len() == 0,
                    _ => false,
                })
    }

    #[test]
    pub fn req_handshake_nozk_nonets() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let nets = vec![];
        let test_msg = NetworkRequest::Handshake(self_peer, nets.clone(), vec![]);
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::Handshake(_,_nets,_zk), _, _) => _zk.len() == 0 && nets.len() == 0,
                    _ => false,
                })
    }

    #[test]
    pub fn req_handshake_no_nets() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let nets = vec![];
        let test_zk = String::from("Random zk data");
        let test_msg = NetworkRequest::Handshake(self_peer, nets.clone(), test_zk.as_bytes().to_vec());
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::Handshake(_,_nets,_zk), _, _) => _zk == test_zk.as_bytes().to_vec() && nets == _nets,
                    _ => false,
                })
    }

    #[test]
    pub fn resp_handshake_no_nets() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let nets = vec![];
        let test_zk = String::from("Random zk data");
        let test_msg = NetworkResponse::Handshake(self_peer, nets.clone(), test_zk.as_bytes().to_vec());
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::Handshake(_,_nets,_zk), _, _) => _zk == test_zk.as_bytes().to_vec() && nets == _nets,
                    _ => false,
                })
    }

    #[test]
    pub fn req_handshake_000() {
        let self_peer: P2PPeer = P2PPeer::from(P2PNodeId::from_string(&String::from("c19cd000746763871fae95fcdd4508dfd8bf725f9767be68c3038df183527bb2")).unwrap(), "10.10.10.10".parse().unwrap(), 8888);
        let nets = vec![0, 100];
        let test_zk = String::from("Random zk data");
        let test_msg = NetworkRequest::Handshake(self_peer, nets.clone(), test_zk.as_bytes().to_vec());
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::Handshake(_, _nets,_zk), _, _) => _zk == test_zk.as_bytes().to_vec() && nets == _nets,
                    _ => false,
                })
    }

    #[test]
    pub fn req_get_peers() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let test_msg = NetworkRequest::GetPeers(self_peer);
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::GetPeers(_), _, _) => true,
                    _ => false,
                })
    }

    #[test]
    pub fn req_findnode_test() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let node_id = P2PNodeId::from_ipstring("8.8.8.8:9999".to_string());
        let msg = NetworkRequest::FindNode(self_peer, node_id.clone());
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::FindNode(_, id), _, _) => {
                        id.get_id() == node_id.get_id()
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn resp_findnode_empty_test() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let msg = NetworkResponse::FindNode(self_peer, vec![]);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::FindNode(_, peers), _, _) => {
                        peers.len() == 0
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn resp_findnode_v4_test() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let port: u16 = 9999;
        let ipaddr = IpAddr::from_str("8.8.8.8").unwrap();
        let msg = NetworkResponse::FindNode(self_peer, vec![P2PPeer::new(ipaddr, port)]);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::FindNode(_, peers), _, _) => {
                        peers.len() == 1
                        && peers.get(0).unwrap().ip == ipaddr
                        && peers.get(0).unwrap().port == port
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn resp_findnode_v6_test() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let port: u16 = 9999;
        let ipaddr = IpAddr::from_str("ff80::dead:beaf").unwrap();
        let msg = NetworkResponse::FindNode(self_peer, vec![P2PPeer::new(ipaddr, port)]);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::FindNode(_, peers), _, _) => {
                        peers.len() == 1
                        && peers.get(0).unwrap().ip == ipaddr
                        && peers.get(0).unwrap().port == port
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn resp_findnode_mixed_test() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let port: u16 = 9999;
        let ipaddr1 = IpAddr::from_str("ff80::dead:beaf").unwrap();
        let ipaddr2 = IpAddr::from_str("8.8.8.8").unwrap();
        let msg = NetworkResponse::FindNode(self_peer,
                                            vec![P2PPeer::new(ipaddr1, port),
                                                 P2PPeer::new(ipaddr2, port)]);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::FindNode(_, peers), _, _) => {
                        peers.len() == 2
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn resp_peerslist_v4_test() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let port: u16 = 9999;
        let ipaddr = IpAddr::from_str("8.8.8.8").unwrap();
        let msg = NetworkResponse::PeerList(self_peer, vec![P2PPeer::new(ipaddr, port)]);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::PeerList(_, peers), _, _) => {
                        peers.len() == 1
                        && peers.get(0).unwrap().ip == ipaddr
                        && peers.get(0).unwrap().port == port
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn resp_peerlist_v6_test() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let port: u16 = 9999;
        let ipaddr = IpAddr::from_str("ff80::dead:beaf").unwrap();
        let msg = NetworkResponse::PeerList(self_peer, vec![P2PPeer::new(ipaddr, port)]);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::PeerList(_, peers), _, _) => {
                        peers.len() == 1
                        && peers.get(0).unwrap().ip == ipaddr
                        && peers.get(0).unwrap().port == port
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn resp_peerslist_mixed_test() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let port: u16 = 9999;
        let ipaddr1 = IpAddr::from_str("ff80::dead:beaf").unwrap();
        let ipaddr2 = IpAddr::from_str("8.8.8.8").unwrap();
        let msg = NetworkResponse::PeerList(self_peer,
                                            vec![P2PPeer::new(ipaddr1, port),
                                                 P2PPeer::new(ipaddr2, port)]);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::PeerList(_, peers), _, _) => {
                        peers.len() == 2
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn direct_message_test() {
        let ipaddr = IpAddr::from_str("10.10.10.10").unwrap();
        let port = 9999;
        let self_peer: P2PPeer = P2PPeer::new(ipaddr, port);
        let text_msg = String::from("Hello world!");
        let msg = NetworkPacket::DirectMessage(self_peer,
                                               P2PNodeId::from_ip_port(ipaddr, port),
                                               100,
                                               text_msg.as_bytes().to_vec());
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_, _, nid, msg),
                                                  _,
                                                  _) => text_msg.as_bytes().to_vec() == msg && nid == 100,
                    _ => false,
                })
    }

    #[test]
    pub fn broadcasted_message_test() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let text_msg = String::from("Hello  broadcasted world!");
        let msg = NetworkPacket::BroadcastedMessage(self_peer, 100, text_msg.as_bytes().to_vec());
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_, nid, msg),
                                                  _,
                                                  _) => text_msg.as_bytes().to_vec() == msg && nid == 100,
                    _ => false,
                })
    }

    #[test]
    pub fn req_bannode_test() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let node_id = P2PNodeId::from_ipstring("8.8.8.8:9999".to_string());
        let peer = P2PPeer::from(node_id, IpAddr::from_str("8.8.8.8").unwrap(), 9999);
        let msg = NetworkRequest::BanNode(self_peer, peer.clone());
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::BanNode(_, _peer), _, _) => {
                        _peer == peer
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn req_unbannode_test() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let node_id = P2PNodeId::from_ipstring("8.8.8.8:9999".to_string());
        let peer = P2PPeer::from(node_id, IpAddr::from_str("8.8.8.8").unwrap(), 9999);
        let msg = NetworkRequest::UnbanNode(self_peer, peer.clone());
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(_, _peer), _, _) => {
                        _peer == peer
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn req_joinnetwork_test() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let msg = NetworkRequest::JoinNetwork(self_peer, 100);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::JoinNetwork(_, network_id), _, _) => {
                        network_id == 100
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn req_leavenetwork_test() {
        let self_peer: P2PPeer = P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999);
        let msg = NetworkRequest::LeaveNetwork(self_peer, 100);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::LeaveNetwork(_, network_id), _, _) => {
                        network_id == 100
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn resp_invalid_version() {
        let test_value = "CONCORDIUMP2P0021001".as_bytes();
        let deserialized = NetworkMessage::deserialize(test_value);
        assert!(match deserialized {
                    NetworkMessage::InvalidMessage => true,
                    _ => false,
                })
    }

    #[test]
    pub fn resp_invalid_protocol() {
        let test_value = "CONC0RD1UMP2P0021001".as_bytes();
        let deserialized = NetworkMessage::deserialize(test_value);
        assert!(match deserialized {
                    NetworkMessage::InvalidMessage => true,
                    _ => false,
                })
    }

}
