pub mod counter;
pub mod fails;
pub mod p2p_node_id;
pub mod p2p_peer;

use crate::connection::MessageSendingPriority;
use concordium_common::hybrid_buf::HybridBuf;

use chrono::prelude::*;
use mio::Token;

use std::net::{IpAddr, SocketAddr};

/// This data type is used to queue a request from any thread (like tests, RPC,
/// Cli, etc.), into a node. Please note that any access to internal `socket`
/// *must be executed* inside MIO poll-loop thread.
pub struct NetworkRawRequest {
    pub token:    Token, // It identifies the connection.
    pub data:     HybridBuf,
    pub priority: MessageSendingPriority,
}

#[cfg(feature = "collector")]
pub mod collector_utils;

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

pub use self::{
    p2p_node_id::P2PNodeId,
    p2p_peer::{P2PPeer, P2PPeerBuilder, PeerStats, PeerType, RemotePeer},
};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        network::{
            NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType, NetworkRequest,
            NetworkResponse, PROTOCOL_VERSION,
        },
        p2p::banned_nodes::tests::dummy_ban_node,
    };
    use concordium_common::{hybrid_buf::HybridBuf, serial::serialize_into_buffer, Serial};

    use failure::Fallible;

    use std::{
        collections::HashSet,
        convert::TryFrom,
        str::FromStr,
        sync::{atomic::AtomicU16, Arc, RwLock},
    };

    fn dummy_peer(ip: IpAddr, port: u16) -> RemotePeer {
        RemotePeer {
            id:                 Arc::new(RwLock::new(Some(P2PNodeId::default()))),
            addr:               SocketAddr::new(ip, port),
            peer_external_port: Arc::new(AtomicU16::new(port)),
            peer_type:          PeerType::Node,
        }
    }

    fn self_peer() -> RemotePeer { dummy_peer(IpAddr::from([10, 10, 10, 10]), 9999) }

    const ZK: &[u8] = b"Random zk data";

    macro_rules! create_message {
        ($msg:ident, $msg_type:ident, $peer:expr) => {
            $msg::$msg_type
        };
        ($msg:ident, $msg_type:ident, $peer:expr, $nets:expr) => {
            $msg::$msg_type($nets.clone())
        };
        (
            $msg:ident,
            $msg_type:ident,
            $remote_node_id:expr,
            $remote_port:expr,
            $zk:expr,
            $nets:expr
        ) => {
            $msg::$msg_type($remote_node_id, $remote_port, $zk.clone(), $nets.clone())
        };
    }

    macro_rules! net_assertion {
        ($msg:ident, $msg_type:ident, $deserialized:expr) => {
            assert!(match $deserialized {
                NetworkMessage::$msg($msg::$msg_type, ..) => true,
                _ => false,
            })
        };
        ($msg:ident, $msg_type:ident, $deserialized:expr, $nets:expr) => {{
            match $deserialized {
                NetworkMessage::$msg($msg::$msg_type(nets2), ..) => {
                    assert_eq!($nets, nets2);
                }
                _ => panic!("invalid network message"),
            }
        }};
        ($msg:ident, $msg_type:ident, $deserialized:expr, $zk:expr, $nets:expr) => {{
            match $deserialized {
                NetworkMessage::$msg($msg::$msg_type(_, _, nets2, zk2), ..) => {
                    assert_eq!($zk, zk2);
                    assert_eq!($nets, nets2);
                }
                _ => panic!("invalid network message"),
            }
        }};
    }

    macro_rules! net_test {
        ($msg:ident, $msg_type:ident) => {{
            let test_msg = NetworkMessage::$msg(
                create_message!($msg, $msg_type, self_peer.clone().peer().unwrap()),
                Some(get_current_stamp()),
                None,
            );
            let mut test_msg_data = serialize_into_buffer(&test_msg, 256).unwrap();

            let deserialized = NetworkMessage::deserial(&mut test_msg_data).unwrap();
            net_assertion!($msg, $msg_type, deserialized)
        }};
        ($msg:ident, $msg_type:ident, $nets:expr) => {{
            let nets = $nets;
            let test_msg = NetworkMessage::$msg(
                create_message!($msg, $msg_type, self_peer.clone().peer().unwrap(), nets),
                Some(get_current_stamp()),
                None,
            );
            let mut test_msg_data = serialize_into_buffer(&test_msg, 256).unwrap();

            let deserialized = NetworkMessage::deserial(&mut test_msg_data).unwrap();

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
                create_message!(
                    $msg,
                    $msg_type,
                    self_peer.clone().peer().unwrap().id(),
                    self_peer.clone().peer().unwrap().port(),
                    nets,
                    zk
                ),
                Some(get_current_stamp()),
                None,
            );
            let mut test_msg_data = serialize_into_buffer(&test_msg, 256).unwrap();

            let deserialized = NetworkMessage::deserial(&mut test_msg_data).unwrap();

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
    fn resp_findnode_empty_test() { net_test!(NetworkResponse, PeerList, vec![] as Vec<P2PPeer>) }

    #[test]
    fn resp_findnode_v4_test() {
        net_test!(NetworkResponse, PeerList, vec![dummy_peer(
            IpAddr::from([8, 8, 8, 8]),
            9999
        )
        .peer()
        .unwrap()])
    }

    #[test]
    fn resp_findnode_v6_test() {
        net_test!(NetworkResponse, PeerList, vec![dummy_peer(
            IpAddr::from_str("ff80::dead:beaf").unwrap(),
            9999
        )
        .peer()
        .unwrap()])
    }

    #[test]
    fn resp_findnode_mixed_test() {
        net_test!(NetworkResponse, PeerList, vec![
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
        let text_msg = b"Hello world!";
        let buf = HybridBuf::try_from(&text_msg[..])?;
        let msg = NetworkPacket {
            packet_type: NetworkPacketType::DirectMessage(P2PNodeId::default()),
            network_id:  NetworkId::from(100),
            message:     buf,
        };

        let mut msg_serialized = serialize_into_buffer(&msg, 256)?;
        let mut packet = NetworkPacket::deserial(&mut msg_serialized)?;

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
        let text_msg = b"Hello  broadcasted world!";
        let buf = HybridBuf::try_from(&text_msg[..])?;
        let msg = NetworkPacket {
            packet_type: NetworkPacketType::BroadcastedMessage(vec![]),
            network_id:  NetworkId::from(100),
            message:     buf,
        };

        let mut serialized = serialize_into_buffer(&msg, 256)?;
        let mut packet = NetworkPacket::deserial(&mut serialized)?;

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
        let ping = NetworkMessage::NetworkRequest(NetworkRequest::Ping, None, None);
        let mut ping_data = Vec::try_from(serialize_into_buffer(&ping, 128).unwrap()).unwrap();

        // Force and error in version protocol:
        //  + 13 bytes (PROTOCOL_NAME)
        //  + 1 byte due to endianess (Version is stored as u16)
        ping_data[13 + 1] = (PROTOCOL_VERSION + 1) as u8;

        let mut ping_data = HybridBuf::try_from(ping_data).unwrap();
        let deserialized = NetworkMessage::deserial(&mut ping_data);

        assert!(deserialized.is_err());
    }

    #[test]
    fn resp_invalid_protocol() {
        let ping = NetworkMessage::NetworkRequest(NetworkRequest::Ping, None, None);
        let mut ping_data = Vec::try_from(serialize_into_buffer(&ping, 128).unwrap()).unwrap();

        // Force and error in protocol name:
        ping_data[1] = b'X';

        let mut ping_data = HybridBuf::try_from(ping_data).unwrap();
        let deserialized = NetworkMessage::deserial(&mut ping_data);

        assert!(deserialized.is_err())
    }
}
