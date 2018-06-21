use utils;
use num_bigint::BigUint;
use num_traits::Num;
use std::net::IpAddr;
use std::str::FromStr;

const PROTOCOL_NAME: &'static str = "CONCORDIUMP2P";
const PROTOCOL_VERSION: &'static str = "001";
const PROTOCOL_NODE_ID_LENGTH:usize = 64;

const PROTOCOL_MESSAGE_TYPE_REQUEST_PING: &'static str = "0001";
const PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE: &'static str = "0002";
const PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG: &'static str = "1001";
const PROTOCOL_MESSAGE_TYPE_RESPONSE_FINDNODE: &'static str = "1002";

#[derive(Debug,Clone)]
pub enum NetworkMessage {
    NetworkRequest(NetworkRequest),
    NetworkResponse(NetworkResponse),
    UnknownMessage,
    InvalidMessage
}

impl NetworkMessage {
    pub fn deserialize(bytes: &str) -> NetworkMessage {
        let protocol_name_length = PROTOCOL_NAME.len();
        let protocol_version_length = PROTOCOL_VERSION.len();
        if bytes.len() >= protocol_name_length && bytes[..protocol_name_length] == *PROTOCOL_NAME  {
            if bytes.len() >= protocol_name_length+protocol_version_length && bytes[protocol_name_length..(protocol_name_length+protocol_version_length)] == *PROTOCOL_VERSION  {
                if bytes.len() < protocol_name_length+protocol_version_length+4 {
                    return NetworkMessage::InvalidMessage;
                }
                let message_type_id = &bytes[(protocol_name_length+protocol_version_length)..(protocol_name_length+protocol_version_length+4)];
                let inner_msg_size = protocol_name_length+protocol_version_length+4;
                match message_type_id as &str{
                    PROTOCOL_MESSAGE_TYPE_REQUEST_PING => NetworkMessage::NetworkRequest(NetworkRequest::Ping),
                    PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG => NetworkMessage::NetworkResponse(NetworkResponse::Pong),
                    PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE => {
                        if bytes.len() != protocol_name_length+protocol_version_length+4+PROTOCOL_NODE_ID_LENGTH {
                            return NetworkMessage::InvalidMessage;
                        }
                        let node_id = &bytes[inner_msg_size..];
                        NetworkMessage::NetworkRequest(NetworkRequest::FindNode(P2PNodeId::from_string(node_id.to_string())))
                    },
                    PROTOCOL_MESSAGE_TYPE_RESPONSE_FINDNODE => {
                        let inner_msg: &str = &bytes[inner_msg_size..];
                        if inner_msg.len() < 3 {
                            return NetworkMessage::InvalidMessage;
                        }
                        let peers_count:Option<usize> = match &inner_msg[..3].parse::<usize>() {
                            Ok(n) => Some(*n),
                            Err(_) => None
                        };
                        if peers_count.is_none() || peers_count.unwrap() == 0 {
                            return NetworkMessage::NetworkResponse(NetworkResponse::FindNode(vec![]));
                        }

                        let mut current_peer_start:usize = 3;
                        let mut peers:Vec<P2PPeer> = vec![];
                        for _ in 0..peers_count.unwrap() {
                            if &inner_msg.len() < &(current_peer_start+3) {
                                return NetworkMessage::InvalidMessage;
                            }
                            match &inner_msg[current_peer_start..(current_peer_start+3)] {
                                "IP4" => {
                                    current_peer_start += 3;
                                    if inner_msg.len() < current_peer_start+12+5 {
                                        return NetworkMessage::InvalidMessage;
                                    }
                                    match IpAddr::from_str(&format!("{}.{}.{}.{}", &inner_msg[current_peer_start..(current_peer_start+3)], &inner_msg[(current_peer_start+3)..(current_peer_start+6)],
                                        &inner_msg[(current_peer_start+6)..(current_peer_start+9)],&inner_msg[(current_peer_start+9)..(current_peer_start+12)])[..]) {
                                        Ok(ip_addr) => {
                                            match inner_msg[(current_peer_start+12)..(current_peer_start+17)].parse::<u16>() {
                                                Ok(port) => {
                                                    peers.push(P2PPeer::new(ip_addr,port));
                                                },
                                                Err(_) => return NetworkMessage::InvalidMessage
                                            }
                                        },
                                        Err(_) => return NetworkMessage::InvalidMessage
                                    };
                                    current_peer_start += 12+5;
                                },
                                "IP6" => {
                                    current_peer_start += 3;
                                    if inner_msg.len() < current_peer_start+32+5 {
                                        return NetworkMessage::InvalidMessage;
                                    }
                                     match IpAddr::from_str(&format!("{}:{}:{}:{}:{}:{}:{}:{}", &inner_msg[current_peer_start..(current_peer_start+4)], &inner_msg[(current_peer_start+4)..(current_peer_start+8)],
                                    &inner_msg[(current_peer_start+8)..(current_peer_start+12)],&inner_msg[(current_peer_start+12)..(current_peer_start+16)],&inner_msg[(current_peer_start+16)..(current_peer_start+20)],
                                    &inner_msg[(current_peer_start+20)..(current_peer_start+24)],&inner_msg[(current_peer_start+24)..(current_peer_start+28)],&inner_msg[(current_peer_start+28)..(current_peer_start+32)])[..]) {
                                        Ok(ip_addr) => {
                                            match inner_msg[(current_peer_start+32)..(current_peer_start+37)].parse::<u16>() {
                                                Ok(port) => {
                                                    peers.push(P2PPeer::new(ip_addr,port));
                                                },
                                                Err(_) => return NetworkMessage::InvalidMessage
                                            }
                                        },
                                        Err(_) => return NetworkMessage::InvalidMessage
                                    };
                                    current_peer_start += 32+5;
                                },
                                _ => {}
                            };
                        }
                        return NetworkMessage::NetworkResponse(NetworkResponse::FindNode(peers));
                    }
                    _ => NetworkMessage::UnknownMessage
                }
            } else { 
                NetworkMessage::InvalidMessage
            }
        } else {
            NetworkMessage::InvalidMessage
        }
    }
}

#[derive(Debug,Clone)]
pub enum NetworkRequest {
    Ping,
    FindNode(P2PNodeId)
}

impl NetworkRequest {
     pub fn serialize(&self) -> String {
        match self {
            NetworkRequest::Ping => format!("{}{}{}", PROTOCOL_NAME, PROTOCOL_VERSION, PROTOCOL_MESSAGE_TYPE_REQUEST_PING),
            NetworkRequest::FindNode(id) => format!("{}{}{}{:x}", PROTOCOL_NAME, PROTOCOL_VERSION, PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE,id.get_id() )
        }
    }
}

#[derive(Debug,Clone)]
pub enum NetworkResponse {
    Pong,
    FindNode(Vec<P2PPeer>)
}

impl NetworkResponse {
     pub fn serialize(&self) -> String {
        match self {
            NetworkResponse::Pong => format!("{}{}{}", PROTOCOL_NAME, PROTOCOL_VERSION, PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG),
            NetworkResponse::FindNode(peers) => {
                let mut buffer = String::new();
                for peer in peers.iter() {
                    match peer.ip {
                        IpAddr::V4(ip4) => {
                            buffer.push_str(&format!("IP4{:03}{:03}{:03}{:03}{:05}", ip4.octets()[0], ip4.octets()[1], ip4.octets()[2], ip4.octets()[3], peer.port)[..]);
                        },
                        IpAddr::V6(ip6) => {
                            buffer.push_str(&format!("IP6{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:05}", ip6.octets()[0], ip6.octets()[1], ip6.octets()[2], ip6.octets()[3], ip6.octets()[4], ip6.octets()[5], ip6.octets()[6], ip6.octets()[7],ip6.octets()[8], ip6.octets()[9], ip6.octets()[10], ip6.octets()[11], ip6.octets()[12], ip6.octets()[13], ip6.octets()[14], ip6.octets()[15], peer.port)[..]);
                        },
                    };
                }
                format!("{}{}{}{:03}{}", PROTOCOL_NAME, PROTOCOL_VERSION, PROTOCOL_MESSAGE_TYPE_RESPONSE_FINDNODE, peers.len(), buffer)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct P2PPeer {
    ip: IpAddr,
    port: u16,
    id: P2PNodeId,
}

impl P2PPeer {
    pub fn new(ip: IpAddr, port: u16) -> Self {
        P2PPeer {
            ip,
            port,
            id: P2PNodeId::from_ip_port(ip, port),
        }
    }
}

#[derive(Debug, Clone)]
pub struct P2PNodeId {
    id: BigUint,
}

impl P2PNodeId {
    pub fn from_string(sid: String) -> P2PNodeId {
        P2PNodeId {
            id: match BigUint::from_str_radix(&sid, 16) {
                Ok(x) => {
                    x
                },
                Err(_) => {
                    panic!("Couldn't convert ID from hex to biguint")
                }
            }
        }
    }

    pub fn get_id(&self) -> &BigUint {
        &self.id
    }

    pub fn to_string(self) -> String {
        format!("{:x}", self.id)
    }

    pub fn from_ip_port(ip: IpAddr, port: u16) -> P2PNodeId {
        let ip_port = format!("{}:{}", ip, port);
        P2PNodeId::from_string(utils::to_hex_string(utils::sha256(&ip_port)))
    }

    pub fn from_ipstring(ip_port: String) -> P2PNodeId {
        P2PNodeId::from_string(utils::to_hex_string(utils::sha256(&ip_port)))
    }
}

#[cfg(test)]
mod tests {
    use common::*;
    #[test]
    pub fn req_ping_test() {
        const TEST_VALUE:&str = "CONCORDIUMP2P0010001";
        let test_msg = NetworkRequest::Ping;
        let serialized_val = test_msg.serialize();
        assert_eq!(TEST_VALUE, serialized_val );
        let deserialized = NetworkMessage::deserialize(&serialized_val[..]);
        assert! ( match deserialized {
            NetworkMessage::NetworkRequest(NetworkRequest::Ping) => true,
            _ => false
        } )
    }

    #[test]
    pub fn resp_pong_test() {
        const TEST_VALUE:&str = "CONCORDIUMP2P0011001";
        let test_msg = NetworkResponse::Pong;
        let serialized_val = test_msg.serialize();
        assert_eq!(TEST_VALUE, serialized_val );
        let deserialized = NetworkMessage::deserialize(&serialized_val[..]);
        assert! ( match deserialized {
            NetworkMessage::NetworkResponse(NetworkResponse::Pong) => true,
            _ => false
        } )
    }

    #[test]
    pub fn req_findnode_test() {
        const TEST_VALUE: &str = "CONCORDIUMP2P0010002da8b507f3e99f5ba979c4db6d65719add14884d581e0565fb8a7fb1a7fc7a54b";
        let node_id = P2PNodeId::from_ipstring("8.8.8.8:9999".to_string());
        let msg = NetworkRequest::FindNode(node_id.clone());
        let serialized = msg.serialize();
        assert_eq!(TEST_VALUE, serialized);
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert! ( match deserialized {
            NetworkMessage::NetworkRequest(NetworkRequest::FindNode(id)) => id.get_id() == node_id.get_id(),
            _ => false
        } )
    }

    #[test]
    pub fn resp_findnode_empty_test() {
        const TEST_VALUE: &str = "CONCORDIUMP2P0011002000";
        let msg = NetworkResponse::FindNode(vec![]);
        let serialized = msg.serialize();
        assert_eq!(TEST_VALUE, serialized);
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert! ( match deserialized {
            NetworkMessage::NetworkResponse(NetworkResponse::FindNode(peers)) => peers.len() == 0 ,
            _ => false
        } )
    }

    #[test]
    pub fn resp_findnode_v4_test() {
        const TEST_VALUE: &str = "CONCORDIUMP2P0011002001IP400800800800809999";
        let port:u16 = 9999;
        let ipaddr = IpAddr::from_str("8.8.8.8").unwrap();
        let msg = NetworkResponse::FindNode(vec![P2PPeer::new(ipaddr,port)]);
        let serialized = msg.serialize();
        assert_eq!(TEST_VALUE, serialized);
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert! ( match deserialized {
            NetworkMessage::NetworkResponse(NetworkResponse::FindNode(peers)) => peers.len() == 1 && peers.get(0).unwrap().ip == ipaddr && peers.get(0).unwrap().port == port ,
            _ => false
        } )
    }

    #[test]
    pub fn resp_findnode_v6_test() {
        const TEST_VALUE: &str = "CONCORDIUMP2P0011002001IP6ff8000000000000000000000deadbeaf09999";
        let port:u16 = 9999;
        let ipaddr = IpAddr::from_str("ff80::dead:beaf").unwrap();  
        let msg = NetworkResponse::FindNode(vec![P2PPeer::new(ipaddr,port)]);
        let serialized = msg.serialize();
        assert_eq!(TEST_VALUE, serialized);
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert! ( match deserialized {
            NetworkMessage::NetworkResponse(NetworkResponse::FindNode(peers)) =>  peers.len() == 1 && peers.get(0).unwrap().ip == ipaddr && peers.get(0).unwrap().port == port,
            _ => false
        } )
    }
    
    #[test]
    pub fn resp_findnode_mixed_test() {
        const TEST_VALUE: &str = "CONCORDIUMP2P0011002002IP6ff8000000000000000000000deadbeaf09999IP400800800800809999";
        let port:u16 = 9999;
        let ipaddr1 = IpAddr::from_str("ff80::dead:beaf").unwrap();  
        let ipaddr2 = IpAddr::from_str("8.8.8.8").unwrap();
        let msg = NetworkResponse::FindNode(vec![P2PPeer::new(ipaddr1,port), P2PPeer::new(ipaddr2, port)]);
        let serialized = msg.serialize();
        assert_eq!(TEST_VALUE, serialized);
        let deserialized = NetworkMessage::deserialize(&serialized[..]);
        assert! ( match deserialized {
            NetworkMessage::NetworkResponse(NetworkResponse::FindNode(peers)) =>  peers.len() == 2,
            _ => false
        } )
    }

    #[test]
    pub fn resp_invalid_version() {
        const TEST_VALUE:&str = "CONCORDIUMP2P0021001";
        let deserialized = NetworkMessage::deserialize(TEST_VALUE);
        assert! ( match deserialized {
            NetworkMessage::InvalidMessage => true,
            _ => false
        } )
    }

    #[test]
    pub fn resp_invalid_protocol() {
        const TEST_VALUE:&str = "CONC0RD1UMP2P0021001";
        let deserialized = NetworkMessage::deserialize(TEST_VALUE);
        assert! ( match deserialized {
            NetworkMessage::InvalidMessage => true,
            _ => false
        } )
    }

    #[test]
    pub fn resp_unknown_message() {
        const TEST_VALUE:&str = "CONCORDIUMP2P0015555";
        let deserialized = NetworkMessage::deserialize(TEST_VALUE);
        assert! ( match deserialized {
            NetworkMessage::UnknownMessage => true,
            _ => false
        } )
    }
}