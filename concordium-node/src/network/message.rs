pub use super::packet::NetworkPacket;
pub use super::request::NetworkRequest;
pub use super::response::NetworkResponse;

use common::{ P2PPeer, P2PNodeId, ConnectionType, get_current_stamp };
use network::{
    PROTOCOL_NAME, PROTOCOL_VERSION, PROTOCOL_SENT_TIMESTAMP_LENGTH , PROTOCOL_NODE_ID_LENGTH,
    PROTOCOL_MESSAGE_TYPE_REQUEST_PING,
    PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG,
    PROTOCOL_MESSAGE_TYPE_RESPONSE_HANDSHAKE,
    PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE,
    PROTOCOL_MESSAGE_TYPE_REQUEST_GET_PEERS, PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE,
    PROTOCOL_MESSAGE_TYPE_REQUEST_BANNODE,
    PROTOCOL_MESSAGE_TYPE_REQUEST_HANDSHAKE,
    PROTOCOL_MESSAGE_TYPE_REQUEST_UNBANNODE,
    PROTOCOL_MESSAGE_TYPE_REQUEST_JOINNETWORK,
    PROTOCOL_MESSAGE_TYPE_REQUEST_LEAVENETWORK,
    PROTOCOL_MESSAGE_TYPE_RESPONSE_PEERSLIST,
    PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE,
    PROTOCOL_MESSAGE_TYPE_RESPONSE_FINDNODE
};

use std::{ str };
use std::net::{ IpAddr };

#[derive(Debug, Clone)]
pub enum NetworkMessage {
    NetworkRequest(NetworkRequest, Option<u64>, Option<u64>),
    NetworkResponse(NetworkResponse, Option<u64>, Option<u64>),
    NetworkPacket(NetworkPacket, Option<u64>, Option<u64>),
    UnknownMessage,
    InvalidMessage,
}

impl NetworkMessage {
    pub fn deserialize(connection_peer: Option<P2PPeer>,
                       ip: IpAddr,
                       bytes: &[u8])
                       -> NetworkMessage {
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
                        match connection_peer {
                            Some(peer) => NetworkMessage::NetworkRequest(NetworkRequest::Ping(peer), Some(timestamp), Some(get_current_stamp())),
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG => {
                        match connection_peer {
                            Some(peer) => NetworkMessage::NetworkResponse(NetworkResponse::Pong(peer), Some(timestamp), Some(get_current_stamp())),
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_RESPONSE_HANDSHAKE => {
                        if &bytes[inner_msg_size..].len() < &(PROTOCOL_NODE_ID_LENGTH+5) {
                            return NetworkMessage::InvalidMessage;
                        }
                        match str::from_utf8(&bytes[inner_msg_size..(inner_msg_size+PROTOCOL_NODE_ID_LENGTH)]) {
                            Ok(node_id_str) => {
                                match P2PNodeId::from_string(&node_id_str.to_string()) {
                                    Ok(node_id) => {
                                        match str::from_utf8(&bytes[(inner_msg_size+PROTOCOL_NODE_ID_LENGTH)..(inner_msg_size+PROTOCOL_NODE_ID_LENGTH+5)]) {
                                            Ok( port_str ) => {
                                                match port_str.parse::<u16>() {
                                                    Ok( port ) => {
                                                        let sender_size = PROTOCOL_NODE_ID_LENGTH+5;
                                                        if bytes.len() >= sender_size+inner_msg_size+5 {
                                                            match str::from_utf8(&bytes[(inner_msg_size+sender_size)..(5 + sender_size+inner_msg_size)]).unwrap().parse::<usize>() {
                                                                Ok(nids) => {
                                                                    if bytes.len() >= sender_size+inner_msg_size+5+(nids*5) {
                                                                        let mut loaded_nids: Vec<u16> = vec![];
                                                                        for nid_id in 0..nids {
                                                                            match str::from_utf8(&bytes[(inner_msg_size+sender_size+(nid_id*5)+5)..(10 + sender_size+inner_msg_size+(nid_id*5))]).unwrap().parse::<u16>() {
                                                                                Ok(loaded_nid) => loaded_nids.push(loaded_nid),
                                                                                _ => error!("Can't load one of the network ids given")
                                                                            }
                                                                        }
                                                                        if bytes.len() >= loaded_nids.len()*5+5+sender_size+inner_msg_size+10 {
                                                                            match str::from_utf8(&bytes[(loaded_nids.len()*5+5+sender_size+inner_msg_size)..loaded_nids.len()*5+5+sender_size+inner_msg_size+10]).unwrap().parse::<usize>() {
                                                                                Ok(csize) => {
                                                                                    if bytes.len() == loaded_nids.len()*5+5+sender_size+inner_msg_size+csize+10 {
                                                                                        let peer = P2PPeer::from(ConnectionType::Node, node_id,ip.clone(),port);
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
                                                    _ => NetworkMessage::InvalidMessage
                                                }
                                            }
                                            _ => NetworkMessage::InvalidMessage
                                        }
                                    }
                                    _ => NetworkMessage::InvalidMessage
                                }
                            }
                            _ => NetworkMessage::InvalidMessage
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_REQUEST_GET_PEERS => {
                        match connection_peer {
                            Some(peer) => {
                                if bytes[inner_msg_size..].len() > 5 {
                                    match str::from_utf8(&bytes[inner_msg_size..(inner_msg_size+5)]) {
                                        Ok(count_size) => {
                                            match count_size.parse::<u16>() {
                                                Ok(count) => {
                                                    let mut networks: Vec<u16> = vec![];
                                                    for c in 0..count {
                                                        match str::from_utf8(&bytes[(inner_msg_size+5+(c as usize*5))..(inner_msg_size+10+(c as usize*5))]) {
                                                            Ok(nid_str) => {
                                                                match nid_str.parse::<u16>() {
                                                                    Ok(nid) => {
                                                                        networks.push(nid);
                                                                    }
                                                                    _ => return NetworkMessage::InvalidMessage,
                                                                }
                                                            }
                                                            _ => return NetworkMessage::InvalidMessage,
                                                        }
                                                    }
                                                    NetworkMessage::NetworkRequest(NetworkRequest::GetPeers(peer, networks.clone()), Some(timestamp), Some(get_current_stamp()))
                                                }
                                                _ => return NetworkMessage::InvalidMessage,
                                            }
                                        }
                                        _ => return NetworkMessage::InvalidMessage,
                                    }
                                } else {
                                    return NetworkMessage::InvalidMessage;
                                }
                            },
                            _ => return NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_REQUEST_HANDSHAKE => {
                        if &bytes[inner_msg_size..].len() < &(PROTOCOL_NODE_ID_LENGTH+5) {
                            return NetworkMessage::InvalidMessage;
                        }
                        match str::from_utf8(&bytes[inner_msg_size..(inner_msg_size+PROTOCOL_NODE_ID_LENGTH)]) {
                            Ok(node_id_str) => {
                                match P2PNodeId::from_string(&node_id_str.to_string()) {
                                    Ok(node_id) => {
                                        match str::from_utf8(&bytes[(inner_msg_size+PROTOCOL_NODE_ID_LENGTH)..(inner_msg_size+PROTOCOL_NODE_ID_LENGTH+5)]) {
                                            Ok( port_str ) => {
                                                match port_str.parse::<u16>() {
                                                    Ok( port ) => {
                                                        let sender_size = PROTOCOL_NODE_ID_LENGTH+5;
                                                        if bytes.len() >= sender_size+inner_msg_size+5 {
                                                            match str::from_utf8(&bytes[(inner_msg_size+sender_size)..(5 + sender_size+inner_msg_size)]).unwrap().parse::<usize>() {
                                                                Ok(nids) => {
                                                                    if bytes.len() >= sender_size+inner_msg_size+5+(nids*5) {
                                                                        let mut loaded_nids: Vec<u16> = vec![];
                                                                        for nid_id in 0..nids {
                                                                            match str::from_utf8(&bytes[(inner_msg_size+sender_size+(nid_id*5)+5)..(10 + sender_size+inner_msg_size+(nid_id*5))]).unwrap().parse::<u16>() {
                                                                                Ok(loaded_nid) => loaded_nids.push(loaded_nid),
                                                                                _ => error!("Can't load one of the network ids given")
                                                                            }
                                                                        }
                                                                        if bytes.len() >= loaded_nids.len()*5+5+sender_size+inner_msg_size+10 {
                                                                            match str::from_utf8(&bytes[(loaded_nids.len()*5+5+sender_size+inner_msg_size)..loaded_nids.len()*5+5+sender_size+inner_msg_size+10]).unwrap().parse::<usize>() {
                                                                                Ok(csize) => {
                                                                                    if bytes.len() == loaded_nids.len()*5+5+sender_size+inner_msg_size+csize+10 {
                                                                                        let peer = P2PPeer::from(ConnectionType::Node, node_id,ip.clone(),port);
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
                                                    _ => NetworkMessage::InvalidMessage
                                                }
                                            }
                                            _ => NetworkMessage::InvalidMessage
                                        }
                                    }
                                    _ => NetworkMessage::InvalidMessage
                                }
                            }
                            _ => NetworkMessage::InvalidMessage
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE => {
                        match connection_peer {
                            Some(sender) => {
                                if bytes.len() != inner_msg_size + PROTOCOL_NODE_ID_LENGTH {
                                    return NetworkMessage::InvalidMessage;
                                }
                                let node_id = str::from_utf8(&bytes[(inner_msg_size)..]).unwrap();

                                NetworkMessage::NetworkRequest(NetworkRequest::FindNode(sender, P2PNodeId::from_string(&node_id.to_string()).unwrap()), Some(timestamp), Some(get_current_stamp()))
                            }
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_REQUEST_BANNODE => {
                        match connection_peer {
                            Some(sender) => {
                                let node_data = P2PPeer::deserialize(str::from_utf8(&bytes[(inner_msg_size)..]).unwrap());
                                match node_data {
                                    Ok(node_info) => NetworkMessage::NetworkRequest(NetworkRequest::BanNode(sender, node_info), Some(timestamp), Some(get_current_stamp())),
                                    _ => NetworkMessage::InvalidMessage,
                                }
                            }
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_REQUEST_UNBANNODE => {
                        match connection_peer {
                            Some(sender) => {
                                let node_data = P2PPeer::deserialize(str::from_utf8(&bytes[(inner_msg_size)..]).unwrap());
                                match node_data {
                                    Ok(node_info) => NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(sender, node_info), Some(timestamp), Some(get_current_stamp())),
                                    _ => NetworkMessage::InvalidMessage,
                                }
                            }
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_REQUEST_JOINNETWORK => {
                        match connection_peer {
                            Some(sender) => {
                                if bytes[(inner_msg_size)..].len() == 5 {
                                    match str::from_utf8(&bytes[inner_msg_size..]).unwrap().parse::<u16>() {
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
                        match connection_peer {
                            Some(sender) => {
                                if bytes[(inner_msg_size)..].len() == 5 {
                                    match str::from_utf8(&bytes[inner_msg_size..]).unwrap().parse::<u16>() {
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
                        match connection_peer {
                            Some(sender) => {
                                if inner_msg.len() < 3 {
                                    return NetworkMessage::InvalidMessage;
                                }
                                let peers_count: Option<usize> = match str::from_utf8(&inner_msg[..3]).unwrap().parse::<usize>() {
                                    Ok(n) => Some(n),
                                    Err(_) => None,
                                };
                                if peers_count.is_none() || peers_count.unwrap() == 0 {
                                    return NetworkMessage::NetworkResponse(NetworkResponse::FindNode(sender, vec![]), Some(timestamp), Some(get_current_stamp()));
                                }

                                let mut current_peer_start: usize = 3;
                                let mut peers: Vec<P2PPeer> = vec![];
                                for _ in 0..peers_count.unwrap() {
                                    match P2PPeer::deserialize(str::from_utf8(&inner_msg[current_peer_start..]).unwrap()) {
                                        Ok(peer) => {
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
                        match connection_peer {
                            Some(sender) => {
                                if inner_msg.len() < 3 {
                                    return NetworkMessage::InvalidMessage;
                                }
                                let peers_count: Option<usize> = match str::from_utf8(&inner_msg[..3]).unwrap().parse::<usize>() {
                                    Ok(n) => Some(n),
                                    Err(_) => None,
                                };
                                if peers_count.is_none() || peers_count.unwrap() == 0 {
                                    return NetworkMessage::NetworkResponse(NetworkResponse::PeerList(sender, vec![]), Some(timestamp), Some(get_current_stamp()));
                                }

                                let mut current_peer_start: usize = 3;
                                let mut peers: Vec<P2PPeer> = vec![];
                                for _ in 0..peers_count.unwrap() {
                                    match P2PPeer::deserialize(str::from_utf8(&inner_msg[current_peer_start..]).unwrap()) {
                                        Ok(peer) => {
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
                        match connection_peer {
                            Some(peer) => {
                                if bytes[inner_msg_size..].len() < (15 + PROTOCOL_NODE_ID_LENGTH) {
                                    return NetworkMessage::InvalidMessage;
                                }
                                let remainer = inner_msg_size;
                                let receiver_id = P2PNodeId::from_string(&(str::from_utf8(&bytes[remainer..(remainer + PROTOCOL_NODE_ID_LENGTH)]).unwrap()).to_string()).unwrap();
                                let msgid = match str::from_utf8(&bytes[(remainer+PROTOCOL_NODE_ID_LENGTH)..(remainer+PROTOCOL_NODE_ID_LENGTH+64)]) {
                                    Ok(id) => id.to_string(),
                                    _ => return NetworkMessage::InvalidMessage
                                };
                                let network_id = match str::from_utf8(&bytes[(remainer+PROTOCOL_NODE_ID_LENGTH+64)..(remainer+PROTOCOL_NODE_ID_LENGTH+5+64)]).unwrap().parse::<u16>() {
                                    Ok(nid) => nid,
                                    _ => 0,
                                };
                                match str::from_utf8(&bytes[(remainer + PROTOCOL_NODE_ID_LENGTH + 5 +64)..(remainer + 15 + 64 + PROTOCOL_NODE_ID_LENGTH)]).unwrap().parse::<usize>() {
                                    Ok(csize) => {
                                        if bytes[(remainer + PROTOCOL_NODE_ID_LENGTH + 15+64)..].len() != csize {
                                            return NetworkMessage::InvalidMessage;
                                        }
                                        return NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(peer, msgid, receiver_id, network_id, bytes[(remainer + PROTOCOL_NODE_ID_LENGTH + 15 +64)..(remainer + PROTOCOL_NODE_ID_LENGTH + 15 +64 + csize)].to_vec()), Some(timestamp), Some(get_current_stamp()));
                                    }
                                    Err(_) => return NetworkMessage::InvalidMessage,
                                }
                            }
                            _ => NetworkMessage::InvalidMessage,
                        }
                    }
                    PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE => {
                        match connection_peer {
                            Some(peer) => {
                                if bytes[inner_msg_size..].len() < 15+64 {
                                    return NetworkMessage::InvalidMessage;
                                }
                                let msgid = match str::from_utf8(&bytes[inner_msg_size..(inner_msg_size+64)]) {
                                    Ok(id) => id.to_string(),
                                    _ => return NetworkMessage::InvalidMessage
                                };
                                let network_id = match str::from_utf8(&bytes[(inner_msg_size+64)..(inner_msg_size+64+5)]).unwrap().parse::<u16>() {
                                    Ok(nid) => nid,
                                    _ => 0,
                                };
                                match str::from_utf8(&bytes[(inner_msg_size  + 64+5)..(inner_msg_size  + 64+5+10)]).unwrap().parse::<usize>() {
                                    Ok(csize) => {
                                        if bytes[(inner_msg_size  + 64 +5+10)..].len() != csize {
                                            return NetworkMessage::InvalidMessage;
                                        }
                                        return NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(peer,msgid, network_id, bytes[(inner_msg_size + 64 + 5 +10)..(inner_msg_size + 64 + 5 + 10  + csize)].to_vec()), Some(timestamp), Some(get_current_stamp()));
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
