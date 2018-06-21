use utils;
use num_bigint::BigUint;
use num_traits::Num;
use std::net::IpAddr;

const PROTOCOL_NAME: &'static str = "CONCORDIUMP2P";
const PROTOCOL_VERSION: &'static str = "001";

const PROTOCOL_MESSAGE_TYPE_REQUEST_PING: &'static str = "0001";
const PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG: &'static str = "1001";

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
        if( bytes[..protocol_name_length] == *PROTOCOL_NAME ) {
            if( bytes[protocol_name_length..(protocol_name_length+protocol_version_length)] == *PROTOCOL_VERSION ) {
                let message_type_id = &bytes[(protocol_name_length+protocol_version_length)..(protocol_name_length+protocol_version_length+4)];
                match message_type_id as &str{
                    PROTOCOL_MESSAGE_TYPE_REQUEST_PING => NetworkMessage::NetworkRequest(NetworkRequest::Ping),
                    PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG => NetworkMessage::NetworkResponse(NetworkResponse::Pong),
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
            Ping => format!("{}{}{}", PROTOCOL_NAME, PROTOCOL_VERSION, PROTOCOL_MESSAGE_TYPE_REQUEST_PING),
            _ => "INVALID".to_string()
        }
    }
}

#[derive(Debug,Clone)]
pub enum NetworkResponse {
    Pong,
    FindNode(Vec<(P2PNodeId,u8)>)
}

impl NetworkResponse {
     pub fn serialize(&self) -> String {
        match self {
            Pong => format!("{}{}{}", PROTOCOL_NAME, PROTOCOL_VERSION, PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG),
            _ => "INVALID".to_string()
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
                Err(e) => {
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
    pub fn reps_invalid_version() {
        const TEST_VALUE:&str = "CONCORDIUMP2P0021001";
        let deserialized = NetworkMessage::deserialize(TEST_VALUE);
        assert! ( match deserialized {
            NetworkMessage::InvalidMessage => true,
            _ => false
        } )
    }

    #[test]
    pub fn reps_invalid_protocol() {
        const TEST_VALUE:&str = "CONC0RD1UMP2P0021001";
        let deserialized = NetworkMessage::deserialize(TEST_VALUE);
        assert! ( match deserialized {
            NetworkMessage::InvalidMessage => true,
            _ => false
        } )
    }

    #[test]
    pub fn reps_unknown_message() {
        const TEST_VALUE:&str = "CONCORDIUMP2P0015555";
        let deserialized = NetworkMessage::deserialize(TEST_VALUE);
        assert! ( match deserialized {
            NetworkMessage::UnknownMessage => true,
            _ => false
        } )
    }
}
