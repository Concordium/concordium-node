pub mod counter;

#[macro_use] pub mod functor;

pub mod fails;

use num_bigint::BigUint;
use num_traits::Num;
use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::net::IpAddr;
use std::str;
use std::str::FromStr;
use failure::{Fallible, bail};
use crate::utils;
use chrono::prelude::*;

use crate::network::{ PROTOCOL_NODE_ID_LENGTH };

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub enum ConnectionType {
    Node,
    Bootstrapper,
}

#[derive(Debug, Clone, Builder)]
#[builder(build_fn(skip))]
pub struct P2PPeer {
    ip: IpAddr,
    port: u16,
    id: P2PNodeId,
    #[builder(setter(skip))]
    last_seen: u64,
    connection_type: ConnectionType,
}

impl P2PPeerBuilder {
    pub fn build(&mut self) -> Fallible<P2PPeer> {
        let id_bc = match &self.id {
            None => {
                if self.ip.is_some() && self.port.is_some() {
                    Ok(Some(P2PNodeId::from_ip_port(self.ip.unwrap(), self.port.unwrap())?))
                } else {
                    Err(fails::EmptyIpPortError)
                }
            },
            Some(id) => {
                if id.id == BigUint::default() {
                    Ok(Some(P2PNodeId::from_ip_port(self.ip.unwrap(), self.port.unwrap())?))
                } else {
                    Ok(None)
                }
            }
        }?;
        if let Some(id) = id_bc {
            self.id(id);
        }
        if self.connection_type.is_some() && self.id.is_some() &&
            self.ip.is_some() && self.port.is_some() {
                Ok(P2PPeer { connection_type: self.connection_type.unwrap(),
                             ip: self.ip.unwrap(),
                             port: self. port.unwrap(),
                             id: self.id.clone().unwrap(),
                             last_seen: get_current_stamp()})
            } else {
                bail!(fails::MissingFieldsError::new(
                    self.connection_type,
                    self.id.clone(),
                    self.ip,
                    self.port
                ))
            }
    }
}

impl P2PPeer {
    pub fn from(connection_type: ConnectionType, id: P2PNodeId, ip: IpAddr, port: u16) -> Self {
        P2PPeer {
            connection_type,
            id,
            ip,
            port,
            last_seen: get_current_stamp(),
        }
    }

    pub fn serialize(&self) -> String {
        match &self.ip {
            IpAddr::V4(ip4) => format!("{:064x}IP4{:03}{:03}{:03}{:03}{:05}",
                self.id.get_id(),
                ip4.octets()[0],
                ip4.octets()[1],
                ip4.octets()[2],
                ip4.octets()[3],
                self.port,
            ),
            IpAddr::V6(ip6) => format!("{:064x}IP6{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}\
                {:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:05}",
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
                self.port
            ),
        }
    }

    pub fn deserialize(buf: &str) -> Fallible<P2PPeer> {
        let ip_start = PROTOCOL_NODE_ID_LENGTH + 3;

        if buf.len() > ip_start {
            let node_id = &buf[..PROTOCOL_NODE_ID_LENGTH];
            let ip_type = &buf[PROTOCOL_NODE_ID_LENGTH..][..3];
            let (ip_addr, port): (IpAddr, u16) = match ip_type {
                "IP4" => {
                    if buf.len() >= PROTOCOL_NODE_ID_LENGTH + 3 + 12 + 5 {
                        let ip_addr = IpAddr::from_str(&format!("{}.{}.{}.{}",
                            &buf[ip_start..][..3],
                            &buf[ip_start + 3..][..3],
                            &buf[ip_start + 6..][..3],
                            &buf[ip_start + 9..][..3]
                        ))?;
                        let port = buf[ip_start + 12..][..5].parse::<u16>()?;
                        (ip_addr, port)
                    } else {
                        bail!(fails::InvalidLengthForIP::new(ip_type.to_string()))
                    }
                }
                "IP6" => {
                    if buf.len() >= PROTOCOL_NODE_ID_LENGTH + 3 + 32 + 5 {
                        let ip_addr = IpAddr::from_str(&format!("{}:{}:{}:{}:{}:{}:{}:{}",
                            &buf[ip_start..][..4],
                            &buf[ip_start..][4..8],
                            &buf[ip_start..][8..12],
                            &buf[ip_start..][12..16],
                            &buf[ip_start..][16..20],
                            &buf[ip_start..][20..24],
                            &buf[ip_start..][24..28],
                            &buf[ip_start..][28..32]
                        ))?;
                        let port = buf[ip_start..][32..37].parse::<u16>()?;
                        (ip_addr, port)
                    } else {
                        bail!(fails::InvalidLengthForIP::new(ip_type.to_string()))
                    }
                }
                _ => bail!(fails::InvalidIpType::new(ip_type.to_string()))
            };
            P2PPeerBuilder::default()
                .id(P2PNodeId::from_string(&node_id.to_string())?)
                .ip(ip_addr)
                .port(port)
                .connection_type(ConnectionType::Node)
                .build()
        } else {
            bail!(fails::InvalidLength)
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
        self.id == other.id()
    }
}

impl Eq for P2PPeer {}

impl Hash for P2PPeer {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Ord for P2PPeer {
    fn cmp(&self, other: &P2PPeer) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl PartialOrd for P2PPeer {
    fn partial_cmp(&self, other: &P2PPeer) -> Option<Ordering> {
        Some(self.id.cmp(&other.id()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct P2PNodeId {
    id: BigUint,
}

impl P2PNodeId {
    pub fn from_string(sid: &str) -> Fallible<P2PNodeId> {
        Ok(P2PNodeId { id: BigUint::from_str_radix(sid, 16)? })
    }

    pub fn get_id(&self) -> &BigUint {
        &self.id
    }

    pub fn from_ip_port(ip: IpAddr, port: u16) -> Fallible<P2PNodeId> {
        let ip_port = format!("{}:{}", ip, port);
        P2PNodeId::from_string(&utils::to_hex_string(&utils::sha256(&ip_port)))
    }

    pub fn from_ipstring(ip_port: String) -> Fallible<P2PNodeId> {
        P2PNodeId::from_string(&utils::to_hex_string(&utils::sha256(&ip_port)))
    }
}

impl fmt::Display for P2PNodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:064x}", self.id)
    }
}

pub fn get_current_stamp() -> u64 {
    Utc::now().timestamp_millis() as u64
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::network::{ NetworkMessage, NetworkRequest, NetworkResponse, NetworkPacket };

    fn dummy_peer(ip: IpAddr, port: u16) -> P2PPeer {
        P2PPeerBuilder::default()
            .connection_type(ConnectionType::Node)
            .ip(ip)
            .port(port)
            .build()
            .unwrap()
    }

    fn self_peer() -> P2PPeer { dummy_peer(IpAddr::from([10, 10, 10, 10]), 9999) }

    fn peer_000() -> P2PPeer {
        P2PPeerBuilder::default()
            .connection_type(ConnectionType::Node)
            .id(P2PNodeId::from_string("c19cd000746763871fae95fcdd4508dfd8bf725f9767be68c3038df183527bb2").unwrap())
            .ip(IpAddr::from([10, 10, 10, 10]))
            .port(8888)
            .build()
            .unwrap()
    }

    const ZK: &[u8] = b"Random zk data";

    macro_rules! create_message {
        ($msg:ident, $msg_type:ident, $peer:expr) => (
            $msg::$msg_type($peer.clone())
        );
        ($msg:ident, $msg_type:ident, $peer:expr, $nets:expr) => (
            $msg::$msg_type($peer.clone(), $nets.clone())
        );
        ($msg:ident, $msg_type:ident, $peer:expr, $zk:expr, $nets:expr) => (
            $msg::$msg_type($peer.clone(), $zk.clone(), $nets.clone())
        )
    }

    macro_rules! net_assertion {
        ($msg:ident, $msg_type:ident, $deserialized:expr) => (
            assert!(match $deserialized {
                NetworkMessage::$msg($msg::$msg_type(_), ..) => true,
                _ => false,
            })
        );
        ($msg:ident, $msg_type:ident, $deserialized:expr, $nets:expr) => ({
            assert!(match $deserialized {
                NetworkMessage::$msg($msg::$msg_type(_, nets2), ..) => $nets == nets2,
                _ => false,
            })
        });
        ($msg:ident, $msg_type:ident, $deserialized:expr, $zk:expr, $nets:expr) => ({
            assert!(match $deserialized {
                NetworkMessage::$msg($msg::$msg_type(_, nets2, zk2), ..) => {
                    $zk == zk2 && $nets == nets2
                }
                _ => false,
            })
        })
    }

    macro_rules! net_test {
        ($msg:ident, $msg_type:ident, $self_peer:expr) => ({
            let self_peer = $self_peer;
            let test_msg = create_message!($msg, $msg_type, self_peer);
            let serialized = test_msg.serialize();
            let self_peer_ip = self_peer.ip();
            let deserialized = NetworkMessage::deserialize(Some(self_peer), self_peer_ip, &serialized);
            net_assertion!($msg, $msg_type, deserialized)
        });
        ($msg:ident, $msg_type:ident, $self_peer:expr, $nets:expr) => ({
            let self_peer = $self_peer;
            let test_msg = create_message!($msg, $msg_type, self_peer, $nets);
            let serialized = test_msg.serialize();
            let self_peer_ip = self_peer.ip();
            let deserialized = NetworkMessage::deserialize(Some(self_peer), self_peer_ip, &serialized);
            net_assertion!($msg, $msg_type, deserialized, $nets)
        });
        ($msg:ident, $msg_type:ident, $self_peer:expr, $zk:expr, $nets:expr) => ({
            let self_peer = $self_peer;
            let zk: Vec<u8> = $zk;
            let nets: Vec<u16> = $nets;
            let test_msg = create_message!($msg, $msg_type, self_peer, nets, zk);
            let serialized = test_msg.serialize();
            let self_peer_ip = self_peer.ip();
            let deserialized = NetworkMessage::deserialize(Some(self_peer), self_peer_ip, &serialized);
            net_assertion!($msg, $msg_type, deserialized, zk, nets)
        })
    }

    #[test]
    fn req_ping_test() { net_test!(NetworkRequest, Ping, self_peer()) }

    #[test]
    fn resp_pong_test() { net_test!(NetworkResponse, Pong, self_peer()) }

    #[test]
    fn resp_handshake() { net_test!(NetworkResponse, Handshake, self_peer(), ZK.to_vec(), vec![0, 100]) }

    #[test]
    fn req_handshake() { net_test!(NetworkRequest, Handshake, self_peer(), ZK.to_vec(), vec![0, 100]) }

    #[test]
    fn req_handshake_000() { net_test!(NetworkRequest, Handshake, peer_000(), ZK.to_vec(), vec![0, 100]) }

    #[test]
    fn resp_handshake_nozk() { net_test!(NetworkResponse, Handshake, self_peer(), vec![], vec![0, 100]) }

    #[test]
    fn req_handshake_nozk() { net_test!(NetworkRequest, Handshake, self_peer(), vec![], vec![0, 100]) }

    #[test]
    fn resp_handshake_nozk_nonets() { net_test!(NetworkResponse, Handshake, self_peer(), vec![], vec![]) }

    #[test]
    fn req_handshake_nozk_nonets() { net_test!(NetworkRequest, Handshake, self_peer(), vec![], vec![]) }

    #[test]
    fn req_handshake_no_nets() { net_test!(NetworkRequest, Handshake, self_peer(), ZK.to_vec(), vec![]) }

    #[test]
    fn resp_handshake_no_nets() { net_test!(NetworkResponse, Handshake, self_peer(), ZK.to_vec(), vec![]) }

    #[test]
    fn req_get_peers() { net_test!(NetworkRequest, GetPeers, self_peer(), vec![100u16, 200]) }

    #[test]
    fn resp_findnode_empty_test() { net_test!(NetworkResponse, FindNode, self_peer(), vec![] as Vec<P2PPeer>) }

    #[test]
    fn req_findnode_test() {
        net_test!(NetworkRequest, FindNode, self_peer(),
            P2PNodeId::from_ipstring("8.8.8.8:9999".to_string()).unwrap()
        )
    }

    #[test]
    fn resp_findnode_v4_test() {
        net_test!(NetworkResponse, FindNode, self_peer(),
            vec![dummy_peer(IpAddr::from([8, 8, 8, 8]), 9999)]
        )
    }

    #[test]
    fn resp_findnode_v6_test() {
        net_test!(NetworkResponse, FindNode, self_peer(),
            vec![dummy_peer(IpAddr::from_str("ff80::dead:beaf").unwrap(), 9999)]
        )
    }

    #[test]
    fn resp_findnode_mixed_test() {
        net_test!(NetworkResponse, FindNode, self_peer(),
            vec![
                dummy_peer(IpAddr::from_str("ff80::dead:beaf").unwrap(), 9999),
                dummy_peer(IpAddr::from([8, 8, 8, 8]), 9999),
            ]
        )
    }

    #[test]
    fn resp_peerslist_v4_test() {
        net_test!(NetworkResponse, PeerList, self_peer(),
            vec![dummy_peer(IpAddr::from([8, 8, 8, 8]), 9999)]
        )
    }

    #[test]
    fn resp_peerlist_v6_test() {
        net_test!(NetworkResponse, PeerList, self_peer(),
            vec![dummy_peer(IpAddr::from_str("ff80::dead:beaf").unwrap(), 9999)]
        )
    }

    #[test]
    fn resp_peerslist_mixed_test() {
        net_test!(NetworkResponse, PeerList, self_peer(),
            vec![
                dummy_peer(IpAddr::from_str("ff80::dead:beaf").unwrap(), 9999),
                dummy_peer(IpAddr::from([8, 8, 8, 8]), 9999),
            ]
        )
    }

    #[test]
    pub fn direct_message_test() {
        let ipaddr = IpAddr::from_str("10.10.10.10").unwrap();
        let port = 9999;
        let self_peer = self_peer();
        let text_msg = b"Hello world!";
        let msg = NetworkPacket::DirectMessage(self_peer.clone(),
                                               NetworkPacket::generate_message_id(),
                                               P2PNodeId::from_ip_port(ipaddr, port).unwrap(),
                                               100,
                                               text_msg.to_vec());
        let serialized = msg.serialize();
        let self_peer_ip = self_peer.ip();
        let deserialized = NetworkMessage::deserialize(Some(self_peer), self_peer_ip, &serialized);
        assert!(match deserialized {
            NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(.., nid, msg), ..) => {
                text_msg.to_vec() == msg && nid == 100
            }
            _ => false,
        })
    }

    #[test]
    pub fn broadcasted_message_test() {
        let self_peer = self_peer();
        let text_msg = b"Hello broadcasted world!";
        let msg = NetworkPacket::BroadcastedMessage(self_peer.clone(),
                                                    NetworkPacket::generate_message_id(),
                                                    100,
                                                    text_msg.to_vec());
        let serialized = msg.serialize();
        let self_peer_ip = self_peer.ip();
        let deserialized = NetworkMessage::deserialize(Some(self_peer), self_peer_ip, &serialized);
        assert!(match deserialized {
            NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(.., nid, msg), ..) => {
                text_msg.to_vec() == msg && nid == 100
            }
            _ => false,
        })
    }

    #[test]
    fn req_bannode_test() {
        net_test!(NetworkRequest, BanNode, self_peer(), dummy_peer(IpAddr::from([8, 8, 8, 8]), 9999))
    }

    #[test]
    fn req_unbannode_test() {
        net_test!(NetworkRequest, UnbanNode, self_peer(), dummy_peer(IpAddr::from([8, 8, 8, 8]), 9999))
    }

    #[test]
    fn req_joinnetwork_test() { net_test!(NetworkRequest, JoinNetwork, self_peer(), 100u16) }

    #[test]
    fn req_leavenetwork_test() { net_test!(NetworkRequest, LeaveNetwork, self_peer(), 100u16) }

    #[test]
    fn resp_invalid_version() {
        let deserialized = NetworkMessage::deserialize(
            None, IpAddr::from([127, 0, 0, 1]), b"CONCORDIUMP2P0021001");
        assert!(match deserialized { NetworkMessage::InvalidMessage => true, _ => false })
    }

    #[test]
    fn resp_invalid_protocol() {
        let deserialized = NetworkMessage::deserialize(
            None, IpAddr::from([127, 0, 0, 1]), b"CONC0RD1UMP2P0021001");
        assert!(match deserialized { NetworkMessage::InvalidMessage => true, _ => false })
    }

    #[test]
    fn test_message_generate() {
        assert!(NetworkPacket::generate_message_id() != NetworkPacket::generate_message_id());
    }
}
