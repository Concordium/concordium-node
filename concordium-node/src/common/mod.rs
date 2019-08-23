pub mod poll_loop;
pub use poll_loop::{process_network_requests, NetworkRawRequest};

pub mod counter;
pub mod p2p_node_id;
pub mod p2p_peer;
pub mod serialization;

pub use self::{
    p2p_node_id::P2PNodeId,
    p2p_peer::{P2PPeer, P2PPeerBuilder, PeerStats},
};

pub mod fails;

use chrono::prelude::*;
use failure::{bail, Error, Fallible};
use std::{
    fmt,
    net::{IpAddr, SocketAddr},
};

use crate::common::serialization::{Deserializable, ReadArchive, Serializable, WriteArchive};

const PEER_TYPE_NODE: u8 = 0;
const PEER_TYPE_BOOTSTRAPPER: u8 = 1;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum PeerType {
    Node,
    Bootstrapper,
}

impl fmt::Display for PeerType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            PeerType::Node => "Node",
            PeerType::Bootstrapper => "Bootstrapper",
        })
    }
}

impl Serializable for PeerType {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        match self {
            PeerType::Node => PEER_TYPE_NODE,
            PeerType::Bootstrapper => PEER_TYPE_BOOTSTRAPPER,
        }
        .serialize(archive)
    }
}

impl Deserializable for PeerType {
    #[inline]
    fn deserialize<A>(archive: &mut A) -> Fallible<PeerType>
    where
        A: ReadArchive, {
        match u8::deserialize(archive)? {
            PEER_TYPE_NODE => Ok(PeerType::Node),
            PEER_TYPE_BOOTSTRAPPER => Ok(PeerType::Bootstrapper),
            _ => bail!("Unsupported PeerType"),
        }
    }
}

// A representation of different stages a remote peer goes through.
// When a new peer is connected to (be it either via `connect()` or
// `accept()` it starts in `PreHandshake` mode, and at this point all
// we have is a specified `PeerType`. When the handshake is then
// completed, the type is upgraded to `PostHandshake` and at this point
// will contain the full `P2PPeer` struct, which also contains the
// `PeerType` carried over from the previous state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RemotePeer {
    PreHandshake(PeerType, SocketAddr),
    PostHandshake(P2PPeer),
}

impl RemotePeer {
    pub fn is_post_handshake(&self) -> bool {
        match self {
            RemotePeer::PostHandshake(_) => true,
            _ => false,
        }
    }

    pub fn post_handshake_peer_or_else<E, F: FnOnce() -> E>(self, err: F) -> Result<P2PPeer, E> {
        match self {
            RemotePeer::PostHandshake(v) => Ok(v),
            _ => Err(err()),
        }
    }

    pub fn promote_to_post_handshake(&self, id: P2PNodeId, addr: SocketAddr) -> Fallible<Self> {
        match *self {
            RemotePeer::PreHandshake(peer_type, addr) => Ok(RemotePeer::PostHandshake(
                P2PPeer::from(peer_type, id, addr),
            )),
            _ => Err(Error::from(fails::RemotePeerAlreadyPromoted::new(id, addr))),
        }
    }

    pub fn peer(self) -> Option<P2PPeer> {
        match self {
            RemotePeer::PostHandshake(peer) => Some(peer),
            _ => None,
        }
    }

    pub fn addr(&self) -> SocketAddr {
        match self {
            RemotePeer::PreHandshake(_, addr) => *addr,
            RemotePeer::PostHandshake(peer) => peer.addr,
        }
    }

    pub fn peer_type(&self) -> PeerType {
        match self {
            RemotePeer::PostHandshake(peer) => peer.peer_type(),
            RemotePeer::PreHandshake(peer_type, ..) => *peer_type,
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

pub fn serialize_addr(addr: SocketAddr) -> String {
    format!("{}{:05}", serialize_ip(addr.ip()), addr.port())
}

pub fn get_current_stamp() -> u64 { Utc::now().timestamp_millis() as u64 }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        common::serialization::{deserialize_from_memory, serialize_into_memory},
        network::{
            NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType, NetworkRequest,
            NetworkResponse, PROTOCOL_VERSION,
        },
        p2p::banned_nodes::tests::dummy_ban_node,
    };
    use concordium_common::hybrid_buf::HybridBuf;
    use std::{collections::HashSet, str::FromStr};

    fn dummy_peer(ip: IpAddr, port: u16) -> RemotePeer {
        RemotePeer::PostHandshake(
            P2PPeerBuilder::default()
                .peer_type(PeerType::Node)
                .addr(SocketAddr::new(ip, port))
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
            match $deserialized {
                NetworkMessage::$msg($msg::$msg_type(_, nets2, zk2), ..) => {
                    assert_eq!($zk, zk2);
                    assert_eq!($nets, nets2);
                }
                _ => panic!("invalid network message"),
            }
        }};
    }

    macro_rules! net_test {
        ($msg:ident, $msg_type:ident) => {{
            let self_peer = self_peer();
            let test_msg = NetworkMessage::$msg(
                create_message!($msg, $msg_type, self_peer.clone().peer().unwrap()),
                Some(get_current_stamp()),
                None,
            );
            let test_msg_data = serialize_into_memory(&test_msg, 256).unwrap();

            let deserialized =
                deserialize_from_memory::<NetworkMessage>(test_msg_data, self_peer.clone())
                    .unwrap();
            net_assertion!($msg, $msg_type, deserialized)
        }};
        ($msg:ident, $msg_type:ident, $nets:expr) => {{
            let self_peer = self_peer();
            let nets = $nets;
            let test_msg = NetworkMessage::$msg(
                create_message!($msg, $msg_type, self_peer.clone().peer().unwrap(), nets),
                Some(get_current_stamp()),
                None,
            );
            let test_msg_data = serialize_into_memory(&test_msg, 256).unwrap();

            let deserialized =
                deserialize_from_memory::<NetworkMessage>(test_msg_data, self_peer.clone())
                    .unwrap();

            net_assertion!($msg, $msg_type, deserialized, nets)
        }};
        ($msg:ident, $msg_type:ident, $zk:expr, $nets:expr) => {{
            let self_peer = self_peer();
            let zk: Vec<u8> = $zk;
            let nets: HashSet<NetworkId> = $nets
                .into_iter()
                .map(|net: u16| NetworkId::from(net))
                .collect();
            let test_msg = NetworkMessage::$msg(
                create_message!($msg, $msg_type, self_peer.clone().peer().unwrap(), nets, zk),
                Some(get_current_stamp()),
                None,
            );
            let test_msg_data = serialize_into_memory(&test_msg, 256).unwrap();

            let deserialized =
                deserialize_from_memory::<NetworkMessage>(test_msg_data, self_peer.clone())
                    .unwrap();

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
        let self_peer = self_peer();
        let text_msg = b"Hello world!";
        let buf = HybridBuf::from(text_msg.to_vec());
        let msg = NetworkPacket {
            packet_type: NetworkPacketType::DirectMessage(P2PNodeId::default()),
            peer:        self_peer.clone().peer().unwrap(),
            message_id:  NetworkPacket::generate_message_id(),
            network_id:  NetworkId::from(100),
            message:     buf,
        };

        let msg_serialized = serialize_into_memory(&msg, 256)?;
        let mut packet =
            deserialize_from_memory::<NetworkPacket>(msg_serialized, self_peer.clone())?;

        if let NetworkPacketType::DirectMessage(..) = packet.packet_type {
            assert_eq!(packet.network_id, NetworkId::from(100));
            assert_eq!(&packet.message.remaining_bytes()?[..], text_msg);
        } else {
            bail!("It should be a direct message");
        }

        Ok(())
    }

    #[test]
    pub fn broadcasted_message_test() -> Fallible<()> {
        let self_peer = self_peer();
        let text_msg = b"Hello  broadcasted world!";
        let buf = HybridBuf::from(text_msg.to_vec());
        let msg = NetworkPacket {
            packet_type: NetworkPacketType::BroadcastedMessage(vec![]),
            peer:        self_peer.clone().peer().unwrap(),
            message_id:  NetworkPacket::generate_message_id(),
            network_id:  NetworkId::from(100),
            message:     buf,
        };

        let serialized = serialize_into_memory(&msg, 256)?;
        let mut packet = deserialize_from_memory::<NetworkPacket>(serialized, self_peer.clone())?;

        if let NetworkPacketType::BroadcastedMessage(..) = packet.packet_type {
            assert_eq!(packet.network_id, NetworkId::from(100));
            assert_eq!(&packet.message.remaining_bytes()?[..], &text_msg[..]);
        } else {
            bail!("Expected broadcast message");
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
        let ping = NetworkMessage::NetworkRequest(
            NetworkRequest::Ping(self_peer().peer().unwrap()),
            None,
            None,
        );
        let mut ping_data = serialize_into_memory(&ping, 128).unwrap();

        // Force and error in version protocol:
        //  + 13 bytes (PROTOCOL_NAME)
        //  + 1 byte due to endianess (Version is stored as u16)
        ping_data[13 + 1] = (PROTOCOL_VERSION + 1) as u8;

        let deserialized = deserialize_from_memory::<NetworkMessage>(ping_data, self_peer());

        assert!(deserialized.is_err());
    }

    #[test]
    fn resp_invalid_protocol() {
        let ping = NetworkMessage::NetworkRequest(
            NetworkRequest::Ping(self_peer().peer().unwrap()),
            None,
            None,
        );
        let mut ping_data = serialize_into_memory(&ping, 128).unwrap();

        // Force and error in protocol name:
        ping_data[1] = b'X';

        let deserialized = deserialize_from_memory::<NetworkMessage>(ping_data, self_peer());

        assert!(deserialized.is_err())
    }

    #[test]
    fn test_message_generate() {
        assert!(NetworkPacket::generate_message_id() != NetworkPacket::generate_message_id());
    }
}
