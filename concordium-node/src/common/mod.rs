pub mod container_view;
pub mod counter;
pub mod ucursor;

pub use self::{container_view::ContainerView, ucursor::UCursor};

#[macro_use]
pub mod functor;

pub mod fails;

use chrono::prelude::*;
use failure::{bail, Fallible};
use rand::{
    self,
    distributions::{Distribution, Uniform},
};
use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    net::IpAddr,
    str::{self, FromStr},
};

use crate::network::{PROTOCOL_NODE_ID_LENGTH, PROTOCOL_PORT_LENGTH};

const PROTOCOL_IP4_LENGTH: usize = 12;
const PROTOCOL_IP6_LENGTH: usize = 32;
pub const PROTOCOL_IP_TYPE_LENGTH: usize = 3;

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum PeerType {
    Node,
    Bootstrapper,
}

#[derive(Debug, Clone, Builder)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
#[builder(build_fn(skip))]
pub struct P2PPeer {
    id: P2PNodeId,
    ip: IpAddr,
    port: u16,
    #[builder(setter(skip))]
    last_seen: u64,
    peer_type: PeerType,
}

// A represetation of different stages a remote peer goes through.
// When a new peer is connected to (be it either via `connect()` or
// `accept()` it starts in `PreHandshake` mode, and at this point all
// we have is a specified `PeerType`. When the handshake is then
// completed, the type is upgraded to `PostHandshake` and at this point
// will contain the full `P2PPeer` struct, which also contains the
// `PeerType` carried over from the previous state.
#[derive(Debug, Clone)]
pub enum RemotePeer {
    PreHandshake(PeerType),
    PostHandshake(P2PPeer),
}

impl RemotePeer {
    pub fn is_post_handshake(&self) -> bool {
        match self {
            RemotePeer::PostHandshake(_) => true,
            _ => false,
        }
    }

    pub fn is_post_handshake_or_else<E, F: FnOnce() -> E>(self, err: F) -> Result<P2PPeer, E> {
        match self {
            RemotePeer::PostHandshake(v) => Ok(v),
            _ => Err(err()),
        }
    }

    pub fn promote_to_post_handshake(
        &self,
        id: P2PNodeId,
        ip: IpAddr,
        port: u16,
    ) -> Fallible<Self> {
        match *self {
            RemotePeer::PreHandshake(peer_type) => Ok(RemotePeer::PostHandshake(P2PPeer::from(
                peer_type, id, ip, port,
            ))),
            _ => bail!(fails::RemotePeerAlreadyPromoted::new(id, ip, port)),
        }
    }

    pub fn peer(self) -> Option<P2PPeer> {
        match self {
            RemotePeer::PostHandshake(peer) => Some(peer),
            _ => None,
        }
    }
}

impl P2PPeerBuilder {
    pub fn build(&mut self) -> Fallible<P2PPeer> {
        let id = self.id.unwrap_or_else(|| P2PNodeId::default());
        self.id(id);

        if self.peer_type.is_some() && self.id.is_some() && self.ip.is_some() && self.port.is_some()
        {
            Ok(P2PPeer {
                peer_type: self.peer_type.unwrap(),
                ip: self.ip.unwrap(),
                port: self.port.unwrap(),
                id,
                last_seen: get_current_stamp(),
            })
        } else {
            bail!(fails::MissingFieldsError::new(
                self.peer_type,
                self.id,
                self.ip,
                self.port
            ))
        }
    }
}

pub fn serialize_ip(ip: IpAddr) -> String {
    match ip {
        IpAddr::V4(ip4) => format!(
            "IP4{:03}{:03}{:03}{:03}",
            ip4.octets()[0],
            ip4.octets()[1],
            ip4.octets()[2],
            ip4.octets()[3],
        ),
        IpAddr::V6(ip6) => format!(
            "IP6{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:\
             02x}{:02x}{:02x}",
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
        ),
    }
}

pub fn serialize_ip_port(ip: IpAddr, p: u16) -> String { format!("{}{:05}", serialize_ip(ip), p) }

pub fn deserialize_ip4(pkt: &mut UCursor) -> Fallible<IpAddr> {
    let min_packet_size = PROTOCOL_IP4_LENGTH;
    ensure!(
        pkt.len() >= pkt.position() + min_packet_size as u64,
        "IPv4 package needs {} bytes",
        min_packet_size
    );

    let pkt_view = pkt.read_into_view(min_packet_size)?;
    let buf = str::from_utf8(pkt_view.as_slice())?;

    // Decode IP v4
    let ip_addr = IpAddr::from_str(&format!(
        "{}.{}.{}.{}",
        &buf[..3],
        &buf[3..6],
        &buf[6..9],
        &buf[9..12]
    ))?;
    Ok(ip_addr)
}

pub fn deserialize_ip6(pkt: &mut UCursor) -> Fallible<IpAddr> {
    let min_packet_size = PROTOCOL_IP6_LENGTH;
    ensure!(
        pkt.len() >= pkt.position() + min_packet_size as u64,
        "IPv6 package needs {} bytes",
        min_packet_size
    );

    let pkt_view = pkt.read_into_view(min_packet_size)?;
    let pkt_view_slice = pkt_view.as_slice();
    let buf = str::from_utf8(pkt_view_slice)?;

    // Decode IP v6
    let ip_addr = IpAddr::from_str(&format!(
        "{}:{}:{}:{}:{}:{}:{}:{}",
        &buf[..4],
        &buf[4..8],
        &buf[8..12],
        &buf[12..16],
        &buf[16..20],
        &buf[20..24],
        &buf[24..28],
        &buf[28..32]
    ))?;
    Ok(ip_addr)
}

pub fn deserialize_ip(pkt: &mut UCursor) -> Fallible<IpAddr> {
    let min_packet_size = PROTOCOL_IP_TYPE_LENGTH;

    ensure!(
        pkt.len() >= pkt.position() + min_packet_size as u64,
        "P2PPeer package needs {} bytes",
        min_packet_size
    );

    let view = pkt.read_into_view(min_packet_size)?;
    let buf = view.as_slice();
    let ip_type = &buf[..PROTOCOL_IP_TYPE_LENGTH];

    match ip_type {
        b"IP4" => deserialize_ip4(pkt),
        b"IP6" => deserialize_ip6(pkt),
        _ => bail!(fails::InvalidIpType::new(
            str::from_utf8(ip_type)?.to_owned()
        )),
    }
}

pub fn deserialize_ip_port(pkt: &mut UCursor) -> Fallible<(IpAddr, u16)> {
    let ip_addr = deserialize_ip(pkt)?;

    let view = pkt.read_into_view(PROTOCOL_PORT_LENGTH)?;
    let buf = view.as_slice();
    // Decode Port
    let port = str::from_utf8(&buf[..PROTOCOL_PORT_LENGTH])?.parse::<u16>()?;
    Ok((ip_addr, port))
}

impl P2PPeer {
    pub fn from(peer_type: PeerType, id: P2PNodeId, ip: IpAddr, port: u16) -> Self {
        P2PPeer {
            peer_type,
            id,
            ip,
            port,
            last_seen: get_current_stamp(),
        }
    }

    pub fn serialize(&self) -> String {
        format!("{}{}", self.id, serialize_ip_port(self.ip, self.port))
    }

    pub fn deserialize(pkt: &mut UCursor) -> Fallible<P2PPeer> {
        let min_packet_size = PROTOCOL_NODE_ID_LENGTH;

        ensure!(
            pkt.len() >= pkt.position() + min_packet_size as u64,
            "P2PPeer package needs {} bytes",
            min_packet_size
        );

        let view = pkt.read_into_view(min_packet_size)?;
        let buf = view.as_slice();

        let node_id = P2PNodeId::from_str(str::from_utf8(&buf[..PROTOCOL_NODE_ID_LENGTH])?)?;
        debug!(
            "deserialized {:?} to {:?}",
            &buf[..PROTOCOL_NODE_ID_LENGTH],
            node_id
        );

        let (ip_addr, port) = deserialize_ip_port(pkt)?;

        P2PPeerBuilder::default()
            .id(node_id)
            .ip(ip_addr)
            .port(port)
            .peer_type(PeerType::Node)
            .build()
    }

    pub fn id(&self) -> P2PNodeId { self.id }

    pub fn ip(&self) -> IpAddr { self.ip }

    pub fn port(&self) -> u16 { self.port }

    pub fn last_seen(&self) -> u64 { self.last_seen }

    pub fn peer_type(&self) -> PeerType { self.peer_type }
}

impl PartialEq for P2PPeer {
    fn eq(&self, other: &P2PPeer) -> bool { self.id == other.id() }
}

impl Eq for P2PPeer {}

impl Hash for P2PPeer {
    fn hash<H: Hasher>(&self, state: &mut H) { self.id.hash(state); }
}

impl Ord for P2PPeer {
    fn cmp(&self, other: &P2PPeer) -> Ordering { self.partial_cmp(other).unwrap() }
}

impl PartialOrd for P2PPeer {
    fn partial_cmp(&self, other: &P2PPeer) -> Option<Ordering> { Some(self.id.cmp(&other.id())) }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct P2PNodeId(pub u64);

impl Default for P2PNodeId {
    fn default() -> Self {
        let mut rng = rand::thread_rng();
        let n = Uniform::from(0..u64::max_value()).sample(&mut rng);
        P2PNodeId(n)
    }
}

impl fmt::Display for P2PNodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:016x}", self.0) }
}

impl P2PNodeId {
    pub fn from_str<T: AsRef<str>>(s: T) -> Fallible<Self> {
        match u64::from_str_radix(s.as_ref(), 16) {
            Ok(n) => Ok(P2PNodeId(n)),
            Err(e) => bail!("Invalid Node Id ({})", e),
        }
    }
}

pub fn get_current_stamp() -> u64 { Utc::now().timestamp_millis() as u64 }

pub fn get_current_stamp_b64() -> String { base64::encode(&get_current_stamp().to_le_bytes()[..]) }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        network::{
            NetworkId, NetworkMessage, NetworkPacket, NetworkPacketBuilder, NetworkPacketType,
            NetworkRequest, NetworkResponse,
        },
        p2p::banned_nodes::tests::dummy_ban_node,
    };
    use std::collections::HashSet;

    fn dummy_peer(ip: IpAddr, port: u16) -> RemotePeer {
        RemotePeer::PostHandshake(
            P2PPeerBuilder::default()
                .peer_type(PeerType::Node)
                .ip(ip)
                .port(port)
                .build()
                .unwrap(),
        )
    }

    fn self_peer() -> RemotePeer { dummy_peer(IpAddr::from([10, 10, 10, 10]), 9999) }

    const ZK: &[u8] = b"Random zk data";

    macro_rules! create_message {
        ($msg:ident, $msg_type:ident, $peer:expr) => {
            $msg::$msg_type($peer.clone())
        };
        ($msg:ident, $msg_type:ident, $peer:expr, $nets:expr) => {
            $msg::$msg_type($peer, $nets.clone())
        };
        ($msg:ident, $msg_type:ident, $peer:expr, $zk:expr, $nets:expr) => {
            $msg::$msg_type($peer.clone(), $zk.clone(), $nets.clone())
        };
    }

    macro_rules! net_assertion {
        ($msg:ident, $msg_type:ident, $deserialized:expr) => {
            assert!(match $deserialized {
                NetworkMessage::$msg($msg::$msg_type(_), ..) => true,
                _ => false,
            })
        };
        ($msg:ident, $msg_type:ident, $deserialized:expr, $nets:expr) => {{
            match $deserialized {
                NetworkMessage::$msg($msg::$msg_type(_, nets2), ..) => {
                    assert_eq!($nets, nets2);
                }
                _ => panic!("invalid network message"),
            }
        }};
        ($msg:ident, $msg_type:ident, $deserialized:expr, $zk:expr, $nets:expr) => {{
            assert!(match $deserialized {
                NetworkMessage::$msg($msg::$msg_type(_, nets2, zk2), ..) => {
                    $zk == zk2 && $nets == nets2
                }
                _ => false,
            })
        }};
    }

    macro_rules! net_test {
        ($msg:ident, $msg_type:ident) => {{
            let self_peer = self_peer();
            let test_msg = create_message!($msg, $msg_type, self_peer.clone().peer().unwrap());
            let serialized = UCursor::from(test_msg.serialize());
            let deserialized = NetworkMessage::deserialize(
                self_peer.clone(),
                self_peer.peer().unwrap().ip(),
                serialized,
            );
            net_assertion!($msg, $msg_type, deserialized)
        }};
        ($msg:ident, $msg_type:ident, $nets:expr) => {{
            let self_peer = self_peer();
            let nets = $nets;
            let test_msg =
                create_message!($msg, $msg_type, self_peer.clone().peer().unwrap(), nets);
            let serialized = UCursor::from(test_msg.serialize());
            let deserialized = NetworkMessage::deserialize(
                self_peer.clone(),
                self_peer.peer().unwrap().ip(),
                serialized,
            );
            net_assertion!($msg, $msg_type, deserialized, nets)
        }};
        ($msg:ident, $msg_type:ident, $zk:expr, $nets:expr) => {{
            let self_peer = self_peer();
            let zk: Vec<u8> = $zk;
            let nets: HashSet<NetworkId> = $nets
                .into_iter()
                .map(|net: u16| NetworkId::from(net))
                .collect();
            let test_msg =
                create_message!($msg, $msg_type, self_peer.clone().peer().unwrap(), nets, zk);
            let serialized = UCursor::from(test_msg.serialize());
            let deserialized = NetworkMessage::deserialize(
                self_peer.clone(),
                self_peer.peer().unwrap().ip(),
                serialized,
            );
            net_assertion!($msg, $msg_type, deserialized, zk, nets)
        }};
    }

    #[test]
    fn req_ping_test() { net_test!(NetworkRequest, Ping) }

    #[test]
    fn resp_pong_test() { net_test!(NetworkResponse, Pong) }

    #[test]
    fn resp_handshake() { net_test!(NetworkResponse, Handshake, ZK.to_vec(), vec![0, 100]) }

    #[test]
    fn req_handshake() { net_test!(NetworkRequest, Handshake, ZK.to_vec(), vec![0, 100]) }

    #[test]
    fn resp_handshake_nozk() { net_test!(NetworkResponse, Handshake, vec![], vec![0, 100]) }

    #[test]
    fn req_handshake_nozk() { net_test!(NetworkRequest, Handshake, vec![], vec![0, 100]) }

    #[test]
    fn resp_handshake_nozk_nonets() { net_test!(NetworkResponse, Handshake, vec![], vec![]) }

    #[test]
    fn req_handshake_nozk_nonets() { net_test!(NetworkRequest, Handshake, vec![], vec![]) }

    #[test]
    fn req_handshake_no_nets() { net_test!(NetworkRequest, Handshake, ZK.to_vec(), vec![]) }

    #[test]
    fn resp_handshake_no_nets() { net_test!(NetworkResponse, Handshake, ZK.to_vec(), vec![]) }

    #[test]
    fn req_get_peers() {
        let networks: HashSet<NetworkId> =
            vec![100u16, 200].into_iter().map(NetworkId::from).collect();
        net_test!(NetworkRequest, GetPeers, networks)
    }

    #[test]
    fn resp_findnode_empty_test() { net_test!(NetworkResponse, FindNode, vec![] as Vec<P2PPeer>) }

    #[test]
    fn resp_findnode_v4_test() {
        net_test!(NetworkResponse, FindNode, vec![dummy_peer(
            IpAddr::from([8, 8, 8, 8]),
            9999
        )
        .peer()
        .unwrap()])
    }

    #[test]
    fn resp_findnode_v6_test() {
        net_test!(NetworkResponse, FindNode, vec![dummy_peer(
            IpAddr::from_str("ff80::dead:beaf").unwrap(),
            9999
        )
        .peer()
        .unwrap()])
    }

    #[test]
    fn resp_findnode_mixed_test() {
        net_test!(NetworkResponse, FindNode, vec![
            dummy_peer(IpAddr::from_str("ff80::dead:beaf").unwrap(), 9999)
                .peer()
                .unwrap(),
            dummy_peer(IpAddr::from([8, 8, 8, 8]), 9999).peer().unwrap(),
        ])
    }

    #[test]
    fn resp_peerslist_v4_test() {
        net_test!(NetworkResponse, PeerList, vec![dummy_peer(
            IpAddr::from([8, 8, 8, 8]),
            9999
        )
        .peer()
        .unwrap()])
    }

    #[test]
    fn resp_peerlist_v6_test() {
        net_test!(NetworkResponse, PeerList, vec![dummy_peer(
            IpAddr::from_str("ff80::dead:beaf").unwrap(),
            9999
        )
        .peer()
        .unwrap()])
    }

    #[test]
    fn resp_peerslist_mixed_test() {
        net_test!(NetworkResponse, PeerList, vec![
            dummy_peer(IpAddr::from_str("ff80::dead:beaf").unwrap(), 9999)
                .peer()
                .unwrap(),
            dummy_peer(IpAddr::from([8, 8, 8, 8]), 9999).peer().unwrap(),
        ])
    }

    #[test]
    pub fn direct_message_test() -> Fallible<()> {
        let ipaddr = IpAddr::from_str("10.10.10.10")?;
        let self_peer = self_peer();
        let text_msg = ContainerView::from(b"Hello world!".to_vec());
        let msg = NetworkPacketBuilder::default()
            .peer(self_peer.clone().peer().unwrap())
            .message_id(NetworkPacket::generate_message_id())
            .network_id(NetworkId::from(100))
            .message(UCursor::build_from_view(text_msg.clone()))
            .build_direct(P2PNodeId::default())?;
        let serialized = msg.serialize();
        let s11n_cursor = UCursor::build_from_view(ContainerView::from(serialized));
        let mut deserialized = NetworkMessage::deserialize(self_peer.clone(), ipaddr, s11n_cursor);

        if let NetworkMessage::NetworkPacket(ref mut packet, ..) = deserialized {
            if let NetworkPacketType::DirectMessage(..) = packet.packet_type {
                assert_eq!(packet.network_id, NetworkId::from(100));
                assert_eq!(packet.message.read_all_into_view()?, text_msg);
            } else {
                bail!("It should be a direct message");
            }
        } else {
            bail!("It should be a network packet message");
        }

        Ok(())
    }

    #[test]
    pub fn broadcasted_message_test() -> Fallible<()> {
        let ipaddr = IpAddr::from_str("10.10.10.10")?;
        let self_peer = self_peer();
        let text_msg = ContainerView::from(b"Hello  broadcasted world!".to_vec());
        let msg = NetworkPacketBuilder::default()
            .peer(self_peer.clone().peer().unwrap())
            .message_id(NetworkPacket::generate_message_id())
            .network_id(NetworkId::from(100))
            .message(UCursor::build_from_view(text_msg.clone()))
            .build_broadcast()?;

        let serialized = msg.serialize();
        let s11n_cursor = UCursor::build_from_view(ContainerView::from(serialized));
        let mut deserialized = NetworkMessage::deserialize(self_peer.clone(), ipaddr, s11n_cursor);

        if let NetworkMessage::NetworkPacket(ref mut packet, ..) = deserialized {
            if let NetworkPacketType::BroadcastedMessage = packet.packet_type {
                assert_eq!(packet.network_id, NetworkId::from(100));
                assert_eq!(packet.message.read_all_into_view()?, text_msg);
            } else {
                bail!("Expected broadcast message");
            }
        } else {
            bail!("Expected network packet message");
        }
        Ok(())
    }

    #[test]
    fn req_banaddr_test() {
        net_test!(
            NetworkRequest,
            BanNode,
            dummy_ban_node(Some(IpAddr::from([8, 8, 8, 8])))
        )
    }

    #[test]
    fn req_unbanaddr_test() {
        net_test!(
            NetworkRequest,
            UnbanNode,
            dummy_ban_node(Some(IpAddr::from([8, 8, 8, 8])))
        )
    }

    #[test]
    fn req_banid_test() { net_test!(NetworkRequest, BanNode, dummy_ban_node(None)) }

    #[test]
    fn req_unbanid_test() { net_test!(NetworkRequest, UnbanNode, dummy_ban_node(None)) }

    #[test]
    fn req_joinnetwork_test() { net_test!(NetworkRequest, JoinNetwork, NetworkId::from(100)) }

    #[test]
    fn req_leavenetwork_test() { net_test!(NetworkRequest, LeaveNetwork, NetworkId::from(100)) }

    #[test]
    fn resp_invalid_version() {
        let deserialized = NetworkMessage::deserialize(
            self_peer(),
            IpAddr::from([127, 0, 0, 1]),
            UCursor::from(b"CONCORDIUMP2P0021001".to_vec()),
        );
        assert!(match deserialized {
            NetworkMessage::InvalidMessage => true,
            _ => false,
        })
    }

    #[test]
    fn resp_invalid_protocol() {
        let deserialized = NetworkMessage::deserialize(
            self_peer(),
            IpAddr::from([127, 0, 0, 1]),
            UCursor::from(b"CONC0RD1UMP2P0021001".to_vec()),
        );
        assert!(match deserialized {
            NetworkMessage::InvalidMessage => true,
            _ => false,
        })
    }

    #[test]
    fn test_message_generate() {
        assert!(NetworkPacket::generate_message_id() != NetworkPacket::generate_message_id());
    }
}
