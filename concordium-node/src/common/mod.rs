use num_bigint::BigUint;
use num_traits::Num;
use std::cmp::Ordering;
use std::net::IpAddr;
use std::str;
use std::str::FromStr;
use time;
use utils;

use network::{ PROTOCOL_NODE_ID_LENGTH };

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub enum ConnectionType {
    Node,
    Bootstrapper,
}

#[derive(Debug, Clone, Hash)]
pub struct P2PPeer {
    ip: IpAddr,
    port: u16,
    id: P2PNodeId,
    last_seen: u64,
    connection_type: ConnectionType,
}

impl P2PPeer {
    pub fn new(connection_type: ConnectionType, ip: IpAddr, port: u16) -> Self {
        P2PPeer { connection_type,
                  ip,
                  port,
                  id: P2PNodeId::from_ip_port(ip, port),
                  last_seen: get_current_stamp(), }
    }

    pub fn from(connection_type: ConnectionType, id: P2PNodeId, ip: IpAddr, port: u16) -> Self {
        P2PPeer { connection_type,
                  id,
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
                                        ip6.octets()[15], self.port)[..])
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
                                                              last_seen: get_current_stamp(),
                                                              connection_type:
                                                                  ConnectionType::Node, })
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
                                                                  last_seen: get_current_stamp(),
                                                                  connection_type:
                                                                      ConnectionType::Node, });
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

    pub fn connection_type(&self) -> ConnectionType {
        self.connection_type
    }
}

impl PartialEq for P2PPeer {
    fn eq(&self, other: &P2PPeer) -> bool {
        self.id.id == other.id().id || (self.port == other.port() && self.ip == other.ip())
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
    
    use network::{ NetworkMessage, NetworkRequest, NetworkResponse, NetworkPacket };


    use common::*;
    #[test]
    pub fn req_ping_test() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let test_msg = NetworkRequest::Ping(self_peer.clone());
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::Ping(_), _, _) => true,
                    _ => false,
                })
    }

    #[test]
    pub fn resp_pong_test() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let test_msg = NetworkResponse::Pong(self_peer.clone());
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::Pong(_), _, _) => true,
                    _ => false,
                })
    }

    #[test]
    pub fn resp_handshake() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let nets = vec![0, 100];
        let test_zk = String::from("Random zk data");
        let test_msg = NetworkResponse::Handshake(self_peer.clone(),
                                                  nets.clone(),
                                                  test_zk.as_bytes().to_vec());
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::Handshake(_, _nets, _zk),
                                                    _,
                                                    _) => {
                        _zk == test_zk.as_bytes().to_vec() && nets == _nets
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn req_handshake() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let nets = vec![0, 100];
        let test_zk = String::from("Random zk data");
        let test_msg =
            NetworkRequest::Handshake(self_peer.clone(), nets.clone(), test_zk.as_bytes().to_vec());
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::Handshake(_, _nets, _zk),
                                                   _,
                                                   _) => {
                        _zk == test_zk.as_bytes().to_vec() && nets == _nets
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn resp_handshake_nozk() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let nets = vec![0, 100];
        let test_msg = NetworkResponse::Handshake(self_peer.clone(), nets.clone(), vec![]);
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::Handshake(_, _nets, _zk),
                                                    _,
                                                    _) => _zk.len() == 0 && nets == _nets,
                    _ => false,
                })
    }

    #[test]
    pub fn req_handshake_nozk() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let nets = vec![0, 100];
        let test_msg = NetworkRequest::Handshake(self_peer.clone(), nets.clone(), vec![]);
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::Handshake(_, _nets, _zk),
                                                   _,
                                                   _) => _zk.len() == 0 && nets == _nets,
                    _ => false,
                })
    }

    #[test]
    pub fn resp_handshake_nozk_nonets() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let nets = vec![];
        let test_msg = NetworkResponse::Handshake(self_peer.clone(), nets.clone(), vec![]);
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::Handshake(_, _nets, _zk),
                                                    _,
                                                    _) => _zk.len() == 0 && _nets.len() == 0,
                    _ => false,
                })
    }

    #[test]
    pub fn req_handshake_nozk_nonets() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let nets = vec![];
        let test_msg = NetworkRequest::Handshake(self_peer.clone(), nets.clone(), vec![]);
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::Handshake(_, _nets, _zk),
                                                   _,
                                                   _) => _zk.len() == 0 && nets.len() == 0,
                    _ => false,
                })
    }

    #[test]
    pub fn req_handshake_no_nets() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let nets = vec![];
        let test_zk = String::from("Random zk data");
        let test_msg =
            NetworkRequest::Handshake(self_peer.clone(), nets.clone(), test_zk.as_bytes().to_vec());
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::Handshake(_, _nets, _zk),
                                                   _,
                                                   _) => {
                        _zk == test_zk.as_bytes().to_vec() && nets == _nets
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn resp_handshake_no_nets() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let nets = vec![];
        let test_zk = String::from("Random zk data");
        let test_msg = NetworkResponse::Handshake(self_peer.clone(),
                                                  nets.clone(),
                                                  test_zk.as_bytes().to_vec());
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::Handshake(_, _nets, _zk),
                                                    _,
                                                    _) => {
                        _zk == test_zk.as_bytes().to_vec() && nets == _nets
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn req_handshake_000() {
        let self_peer: P2PPeer = P2PPeer::from(ConnectionType::Node, P2PNodeId::from_string(&String::from("c19cd000746763871fae95fcdd4508dfd8bf725f9767be68c3038df183527bb2")).unwrap(), "10.10.10.10".parse().unwrap(), 8888);
        let nets = vec![0, 100];
        let test_zk = String::from("Random zk data");
        let test_msg =
            NetworkRequest::Handshake(self_peer.clone(), nets.clone(), test_zk.as_bytes().to_vec());
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::Handshake(_, _nets, _zk),
                                                   _,
                                                   _) => {
                        _zk == test_zk.as_bytes().to_vec() && nets == _nets
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn req_get_peers() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let test_msg = NetworkRequest::GetPeers(self_peer.clone(), vec![100, 200]);
        let serialized_val = test_msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized_val[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::GetPeers(_, networks), _, _) => {
                        true && vec![100, 200] == networks
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn req_findnode_test() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let node_id = P2PNodeId::from_ipstring("8.8.8.8:9999".to_string());
        let msg = NetworkRequest::FindNode(self_peer.clone(), node_id.clone());
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::FindNode(_, id), _, _) => {
                        id.get_id() == node_id.get_id()
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn resp_findnode_empty_test() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let msg = NetworkResponse::FindNode(self_peer.clone(), vec![]);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::FindNode(_, peers), _, _) => {
                        peers.len() == 0
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn resp_findnode_v4_test() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let port: u16 = 9999;
        let ipaddr = IpAddr::from_str("8.8.8.8").unwrap();
        let msg = NetworkResponse::FindNode(self_peer.clone(),
                                            vec![P2PPeer::new(ConnectionType::Node, ipaddr, port)]);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized[..]);
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
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let port: u16 = 9999;
        let ipaddr = IpAddr::from_str("ff80::dead:beaf").unwrap();
        let msg = NetworkResponse::FindNode(self_peer.clone(),
                                            vec![P2PPeer::new(ConnectionType::Node, ipaddr, port)]);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized[..]);
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
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let port: u16 = 9999;
        let ipaddr1 = IpAddr::from_str("ff80::dead:beaf").unwrap();
        let ipaddr2 = IpAddr::from_str("8.8.8.8").unwrap();
        let msg =
            NetworkResponse::FindNode(self_peer.clone(),
                                      vec![P2PPeer::new(ConnectionType::Node, ipaddr1, port),
                                           P2PPeer::new(ConnectionType::Node, ipaddr2, port)]);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkResponse(NetworkResponse::FindNode(_, peers), _, _) => {
                        peers.len() == 2
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn resp_peerslist_v4_test() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let port: u16 = 9999;
        let ipaddr = IpAddr::from_str("8.8.8.8").unwrap();
        let msg = NetworkResponse::PeerList(self_peer.clone(),
                                            vec![P2PPeer::new(ConnectionType::Node, ipaddr, port)]);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized[..]);
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
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let port: u16 = 9999;
        let ipaddr = IpAddr::from_str("ff80::dead:beaf").unwrap();
        let msg = NetworkResponse::PeerList(self_peer.clone(),
                                            vec![P2PPeer::new(ConnectionType::Node, ipaddr, port)]);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized[..]);
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
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let port: u16 = 9999;
        let ipaddr1 = IpAddr::from_str("ff80::dead:beaf").unwrap();
        let ipaddr2 = IpAddr::from_str("8.8.8.8").unwrap();
        let msg =
            NetworkResponse::PeerList(self_peer.clone(),
                                      vec![P2PPeer::new(ConnectionType::Node, ipaddr1, port),
                                           P2PPeer::new(ConnectionType::Node, ipaddr2, port)]);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized[..]);
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
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node, ipaddr, port);
        let text_msg = String::from("Hello world!");
        let msg = NetworkPacket::DirectMessage(self_peer.clone(),
                                               NetworkPacket::generate_message_id(),
                                               P2PNodeId::from_ip_port(ipaddr, port),
                                               100,
                                               text_msg.as_bytes().to_vec());
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_,
                                                                               _,
                                                                               _,
                                                                               nid,
                                                                               msg),
                                                  _,
                                                  _) => {
                        text_msg.as_bytes().to_vec() == msg && nid == 100
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn broadcasted_message_test() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let text_msg = String::from("Hello  broadcasted world!");
        let msg = NetworkPacket::BroadcastedMessage(self_peer.clone(),
                                                    NetworkPacket::generate_message_id(),
                                                    100,
                                                    text_msg.as_bytes().to_vec());
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_,
                                                                                    _,
                                                                                    nid,
                                                                                    msg),
                                                  _,
                                                  _) => {
                        text_msg.as_bytes().to_vec() == msg && nid == 100
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn req_bannode_test() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let node_id = P2PNodeId::from_ipstring("8.8.8.8:9999".to_string());
        let peer = P2PPeer::from(ConnectionType::Node,
                                 node_id,
                                 IpAddr::from_str("8.8.8.8").unwrap(),
                                 9999);
        let msg = NetworkRequest::BanNode(self_peer.clone(), peer.clone());
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::BanNode(_, _peer), _, _) => {
                        _peer == peer
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn req_unbannode_test() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let node_id = P2PNodeId::from_ipstring("8.8.8.8:9999".to_string());
        let peer = P2PPeer::from(ConnectionType::Node,
                                 node_id,
                                 IpAddr::from_str("8.8.8.8").unwrap(),
                                 9999);
        let msg = NetworkRequest::UnbanNode(self_peer.clone(), peer.clone());
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(_, _peer), _, _) => {
                        _peer == peer
                    }
                    _ => false,
                })
    }

    #[test]
    pub fn req_joinnetwork_test() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let msg = NetworkRequest::JoinNetwork(self_peer.clone(), 100);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::JoinNetwork(_, network_id),
                                                   _,
                                                   _) => network_id == 100,
                    _ => false,
                })
    }

    #[test]
    pub fn req_leavenetwork_test() {
        let self_peer: P2PPeer = P2PPeer::new(ConnectionType::Node,
                                              IpAddr::from_str("10.10.10.10").unwrap(),
                                              9999);
        let msg = NetworkRequest::LeaveNetwork(self_peer.clone(), 100);
        let serialized = msg.serialize();
        let deserialized = NetworkMessage::deserialize(Some(self_peer.clone()),
                                                       self_peer.ip().clone(),
                                                       &serialized[..]);
        assert!(match deserialized {
                    NetworkMessage::NetworkRequest(NetworkRequest::LeaveNetwork(_, network_id),
                                                   _,
                                                   _) => network_id == 100,
                    _ => false,
                })
    }

    #[test]
    pub fn resp_invalid_version() {
        let test_value = "CONCORDIUMP2P0021001".as_bytes();
        let deserialized =
            NetworkMessage::deserialize(None, IpAddr::from_str("127.0.0.1").unwrap(), test_value);
        assert!(match deserialized {
                    NetworkMessage::InvalidMessage => true,
                    _ => false,
                })
    }

    #[test]
    pub fn resp_invalid_protocol() {
        let test_value = "CONC0RD1UMP2P0021001".as_bytes();
        let deserialized =
            NetworkMessage::deserialize(None, IpAddr::from_str("127.0.0.1").unwrap(), test_value);
        assert!(match deserialized {
                    NetworkMessage::InvalidMessage => true,
                    _ => false,
                })
    }

    #[test]
    pub fn test_message_generate() {
        assert!(NetworkPacket::generate_message_id() != NetworkPacket::generate_message_id());
    }
}
