pub mod container_view;
pub mod counter;
pub mod ucursor;

pub use self::{container_view::ContainerView, ucursor::UCursor};

#[macro_use]
pub mod functor;

pub mod fails;

use crate::utils;
use chrono::prelude::*;
use failure::{bail, Fallible};
use num_bigint::BigUint;
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
const PROTOCOL_IP_TYPE_LENGTH: usize = 3;

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum ConnectionType {
    Node,
    Bootstrapper,
}

#[derive(Debug, Clone, Builder)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
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
                    Ok(Some(P2PNodeId::from_ip_port(
                        self.ip.unwrap(),
                        self.port.unwrap(),
                    )))
                } else {
                    Err(fails::EmptyIpPortError)
                }
            }
            Some(id) => {
                if id.id == BigUint::default() {
                    Ok(Some(P2PNodeId::from_ip_port(
                        self.ip.unwrap(),
                        self.port.unwrap(),
                    )))
                } else {
                    Ok(None)
                }
            }
        }?;
        if let Some(id) = id_bc {
            self.id(id);
        }
        if self.connection_type.is_some()
            && self.id.is_some()
            && self.ip.is_some()
            && self.port.is_some()
        {
            Ok(P2PPeer {
                connection_type: self.connection_type.unwrap(),
                ip:              self.ip.unwrap(),
                port:            self.port.unwrap(),
                id:              self.id.clone().unwrap(),
                last_seen:       get_current_stamp(),
            })
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

fn deserialize_ip4(pkt: &mut UCursor) -> Fallible<(IpAddr, u16)> {
    let min_packet_size = PROTOCOL_IP4_LENGTH + PROTOCOL_PORT_LENGTH;
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
    // Decode Port
    let port = buf[PROTOCOL_IP4_LENGTH..][..PROTOCOL_PORT_LENGTH].parse::<u16>()?;
    Ok((ip_addr, port))
}

fn deserialize_ip6(pkt: &mut UCursor) -> Fallible<(IpAddr, u16)> {
    let min_packet_size = PROTOCOL_IP6_LENGTH + PROTOCOL_PORT_LENGTH;
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
    // Decode Port
    let port = buf[PROTOCOL_IP6_LENGTH..][..PROTOCOL_PORT_LENGTH].parse::<u16>()?;
    Ok((ip_addr, port))
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
            IpAddr::V4(ip4) => format!(
                "{}IP4{:03}{:03}{:03}{:03}{:05}",
                self.id.to_b64_repr(),
                ip4.octets()[0],
                ip4.octets()[1],
                ip4.octets()[2],
                ip4.octets()[3],
                self.port,
            ),
            IpAddr::V6(ip6) => format!(
                "{}IP6{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:\
                 02x}{:02x}{:02x}{:02x}{:05}",
                self.id.to_b64_repr(),
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

    pub fn deserialize(pkt: &mut UCursor) -> Fallible<P2PPeer> {
        let min_packet_size = PROTOCOL_NODE_ID_LENGTH + PROTOCOL_IP_TYPE_LENGTH;

        ensure!(
            pkt.len() >= pkt.position() + min_packet_size as u64,
            "P2PPeer package needs {} bytes",
            min_packet_size
        );

        let view = pkt.read_into_view(min_packet_size)?;
        let buf = view.as_slice();

        let node_id = P2PNodeId::from_b64_repr(&str::from_utf8(&buf[..PROTOCOL_NODE_ID_LENGTH])?)?;
        let ip_type = &buf[PROTOCOL_NODE_ID_LENGTH..][..PROTOCOL_IP_TYPE_LENGTH];

        let (ip_addr, port) = match ip_type {
            b"IP4" => deserialize_ip4(pkt)?,
            b"IP6" => deserialize_ip6(pkt)?,
            _ => bail!("Unsupported Ip type"),
        };

        P2PPeerBuilder::default()
            .id(node_id)
            .ip(ip_addr)
            .port(port)
            .connection_type(ConnectionType::Node)
            .build()
    }

    pub fn id(&self) -> P2PNodeId { self.id.clone() }

    pub fn ip(&self) -> IpAddr { self.ip }

    pub fn port(&self) -> u16 { self.port }

    pub fn last_seen(&self) -> u64 { self.last_seen }

    pub fn connection_type(&self) -> ConnectionType { self.connection_type }
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
// #[derive(Debug, Clone, Hash, Serialize, Deserialize)]
pub struct P2PNodeId {
    // #[serde(skip)]
    // #[serde(with = "s11n_big_uint")]
    pub id: BigUint,
}

#[cfg(feature = "s11n_serde")]
use serde::ser::{Serialize, SerializeStruct, Serializer};

#[cfg(feature = "s11n_serde")]
impl Serialize for P2PNodeId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer, {
        let id_bytes_le = self.id.to_bytes_le();

        let mut state = serializer.serialize_struct("P2PNodeId", 1)?;
        state.serialize_field("id", &id_bytes_le)?;
        state.end()
    }
}

#[cfg(feature = "s11n_serde")]
use serde::de::{Deserialize, Deserializer, Error, MapAccess, Visitor};

#[cfg(feature = "s11n_serde")]
impl<'de> Deserialize<'de> for P2PNodeId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>, {
        enum Field {
            Id,
        }

        impl<'de> Deserialize<'de> for Field {
            fn deserialize<D>(deserializer: D) -> Result<Field, D::Error>
            where
                D: Deserializer<'de>, {
                struct FieldVisistor;

                impl<'de> Visitor<'de> for FieldVisistor {
                    type Value = Field;

                    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                        formatter.write_str("`id`")
                    }

                    fn visit_str<E>(self, value: &str) -> Result<Field, E>
                    where
                        E: serde::de::Error, {
                        match value {
                            "id" => Ok(Field::Id),
                            _ => Err(serde::de::Error::unknown_field(value, FIELDS)),
                        }
                    }
                }

                deserializer.deserialize_identifier(FieldVisistor)
            }
        }

        struct P2PNodeIdVisitor;

        impl<'de> Visitor<'de> for P2PNodeIdVisitor {
            type Value = P2PNodeId;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("struct P2PNodeId")
            }

            fn visit_map<V>(self, mut map: V) -> Result<P2PNodeId, V::Error>
            where
                V: MapAccess<'de>, {
                let mut id_bytes_le: Option<Vec<u8>> = None;

                while let Some(field) = map.next_key()? {
                    match field {
                        Field::Id => {
                            if id_bytes_le.is_some() {
                                return Err(V::Error::duplicate_field("id"));
                            }
                            id_bytes_le = Some(map.next_value()?);
                        }
                    }
                }

                let id = BigUint::from_bytes_le(
                    id_bytes_le
                        .ok_or_else(|| V::Error::missing_field("id"))?
                        .as_slice(),
                );

                Ok(P2PNodeId { id })
            }
        }

        const FIELDS: &[&str] = &["id"];
        deserializer.deserialize_struct("P2PNodeId", FIELDS, P2PNodeIdVisitor)
    }
}

impl P2PNodeId {
    /// Convert a slice of bytes in `le` into a `P2PNodeId`
    pub fn from_bytes_slice(sid: &[u8]) -> P2PNodeId {
        P2PNodeId {
            id: BigUint::from_bytes_le(sid),
        }
    }

    /// Convert a `String` or `str` in `base64` representation into a
    /// `P2PNodeId`
    pub fn from_b64_repr<T: ?Sized + AsRef<[u8]>>(sid: &T) -> Fallible<P2PNodeId> {
        Ok(P2PNodeId::from_bytes_slice(&base64::decode(&sid)?[..]))
    }

    /// Get the underlying `BigUint`
    pub fn get_id(&self) -> &BigUint { &self.id }

    /// Convert an `ip` and `port` into a `P2PNocdeId`
    pub fn from_ip_port(ip: IpAddr, port: u16) -> P2PNodeId {
        let ip_port = format!("{}:{}", ip, port);
        P2PNodeId::from_ipstring(&ip_port)
    }

    /// Convert the concatenation of `ip` and `port` into a `P2PNodeId`
    pub fn from_ipstring(ip_port: &str) -> P2PNodeId {
        let buf_slice = utils::sha256(&ip_port);
        P2PNodeId::from_bytes_slice(&buf_slice[..])
    }

    /// Get the `base64` encoding of the underlying `BigUint`
    pub fn to_b64_repr(&self) -> String { base64::encode(&self.id.to_bytes_le()) }
}

impl fmt::Display for P2PNodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.to_b64_repr()) }
}

pub fn get_current_stamp() -> u64 { Utc::now().timestamp_millis() as u64 }

pub fn get_current_stamp_b64() -> String { base64::encode(&get_current_stamp().to_le_bytes()[..]) }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::network::{
        NetworkMessage, NetworkPacket, NetworkPacketBuilder, NetworkPacketType, NetworkRequest,
        NetworkResponse,
    };
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
            .id(P2PNodeId::from_b64_repr(&"Cc0Td01Pk/mKDVjJfsQ3rP7P2J0/i3qRAk+2sQz0MtY=").unwrap())
            .ip(IpAddr::from([10, 10, 10, 10]))
            .port(8888)
            .build()
            .unwrap()
    }

    const ZK: &[u8] = b"Random zk data";

    macro_rules! create_message {
        ($msg:ident, $msg_type:ident, $peer:expr) => {
            $msg::$msg_type($peer.clone())
        };
        ($msg:ident, $msg_type:ident, $peer:expr, $nets:expr) => {
            $msg::$msg_type($peer.clone(), $nets.clone())
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
            assert!(match $deserialized {
                NetworkMessage::$msg($msg::$msg_type(_, nets2), ..) => $nets == nets2,
                _ => false,
            })
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
        ($msg:ident, $msg_type:ident, $self_peer:expr) => {{
            let self_peer = $self_peer;
            let test_msg = create_message!($msg, $msg_type, self_peer);
            let serialized = UCursor::from(test_msg.serialize());
            let self_peer_ip = self_peer.ip();
            let deserialized =
                NetworkMessage::deserialize(Some(self_peer), self_peer_ip, serialized);
            net_assertion!($msg, $msg_type, deserialized)
        }};
        ($msg:ident, $msg_type:ident, $self_peer:expr, $nets:expr) => {{
            let self_peer = $self_peer;
            let test_msg = create_message!($msg, $msg_type, self_peer, $nets);
            let serialized = UCursor::from(test_msg.serialize());
            let self_peer_ip = self_peer.ip();
            let deserialized =
                NetworkMessage::deserialize(Some(self_peer), self_peer_ip, serialized);
            net_assertion!($msg, $msg_type, deserialized, $nets)
        }};
        ($msg:ident, $msg_type:ident, $self_peer:expr, $zk:expr, $nets:expr) => {{
            let self_peer = $self_peer;
            let zk: Vec<u8> = $zk;
            let nets: Vec<u16> = $nets;
            let test_msg = create_message!($msg, $msg_type, self_peer, nets, zk);
            let serialized = UCursor::from(test_msg.serialize());
            let self_peer_ip = self_peer.ip();
            let deserialized =
                NetworkMessage::deserialize(Some(self_peer), self_peer_ip, serialized);
            net_assertion!($msg, $msg_type, deserialized, zk, nets)
        }};
    }

    #[test]
    fn req_ping_test() { net_test!(NetworkRequest, Ping, self_peer()) }

    #[test]
    fn resp_pong_test() { net_test!(NetworkResponse, Pong, self_peer()) }

    #[test]
    fn resp_handshake() {
        net_test!(NetworkResponse, Handshake, self_peer(), ZK.to_vec(), vec![
            0, 100
        ])
    }

    #[test]
    fn req_handshake() {
        net_test!(NetworkRequest, Handshake, self_peer(), ZK.to_vec(), vec![
            0, 100
        ])
    }

    #[test]
    fn req_handshake_000() {
        net_test!(NetworkRequest, Handshake, peer_000(), ZK.to_vec(), vec![
            0, 100
        ])
    }

    #[test]
    fn resp_handshake_nozk() {
        net_test!(NetworkResponse, Handshake, self_peer(), vec![], vec![
            0, 100
        ])
    }

    #[test]
    fn req_handshake_nozk() {
        net_test!(NetworkRequest, Handshake, self_peer(), vec![], vec![0, 100])
    }

    #[test]
    fn resp_handshake_nozk_nonets() {
        net_test!(NetworkResponse, Handshake, self_peer(), vec![], vec![])
    }

    #[test]
    fn req_handshake_nozk_nonets() {
        net_test!(NetworkRequest, Handshake, self_peer(), vec![], vec![])
    }

    #[test]
    fn req_handshake_no_nets() {
        net_test!(NetworkRequest, Handshake, self_peer(), ZK.to_vec(), vec![])
    }

    #[test]
    fn resp_handshake_no_nets() {
        net_test!(NetworkResponse, Handshake, self_peer(), ZK.to_vec(), vec![])
    }

    #[test]
    fn req_get_peers() { net_test!(NetworkRequest, GetPeers, self_peer(), vec![100u16, 200]) }

    #[test]
    fn resp_findnode_empty_test() {
        net_test!(NetworkResponse, FindNode, self_peer(), vec![]
            as Vec<P2PPeer>)
    }

    #[test]
    fn req_findnode_test() {
        net_test!(
            NetworkRequest,
            FindNode,
            self_peer(),
            P2PNodeId::from_ipstring("8.8.8.8:9999")
        )
    }

    #[test]
    fn resp_findnode_v4_test() {
        net_test!(NetworkResponse, FindNode, self_peer(), vec![dummy_peer(
            IpAddr::from([8, 8, 8, 8]),
            9999
        )])
    }

    #[test]
    fn resp_findnode_v6_test() {
        net_test!(NetworkResponse, FindNode, self_peer(), vec![dummy_peer(
            IpAddr::from_str("ff80::dead:beaf").unwrap(),
            9999
        )])
    }

    #[test]
    fn resp_findnode_mixed_test() {
        net_test!(NetworkResponse, FindNode, self_peer(), vec![
            dummy_peer(IpAddr::from_str("ff80::dead:beaf").unwrap(), 9999),
            dummy_peer(IpAddr::from([8, 8, 8, 8]), 9999),
        ])
    }

    #[test]
    fn resp_peerslist_v4_test() {
        net_test!(NetworkResponse, PeerList, self_peer(), vec![dummy_peer(
            IpAddr::from([8, 8, 8, 8]),
            9999
        )])
    }

    #[test]
    fn resp_peerlist_v6_test() {
        net_test!(NetworkResponse, PeerList, self_peer(), vec![dummy_peer(
            IpAddr::from_str("ff80::dead:beaf").unwrap(),
            9999
        )])
    }

    #[test]
    fn resp_peerslist_mixed_test() {
        net_test!(NetworkResponse, PeerList, self_peer(), vec![
            dummy_peer(IpAddr::from_str("ff80::dead:beaf").unwrap(), 9999),
            dummy_peer(IpAddr::from([8, 8, 8, 8]), 9999),
        ])
    }

    #[test]
    pub fn direct_message_test() -> Fallible<()> {
        let ipaddr = IpAddr::from_str("10.10.10.10")?;
        let port = 9999;
        let self_peer = self_peer();
        let text_msg = ContainerView::from(b"Hello world!".to_vec());
        let msg = NetworkPacketBuilder::default()
            .peer(self_peer.clone())
            .message_id(NetworkPacket::generate_message_id())
            .network_id(100)
            .message(UCursor::build_from_view(text_msg.clone()))
            .build_direct(P2PNodeId::from_ip_port(ipaddr, port))?;
        let serialized = msg.serialize();
        let s11n_cursor = UCursor::build_from_view(ContainerView::from(serialized));
        let mut deserialized =
            NetworkMessage::deserialize(Some(self_peer.clone()), ipaddr, s11n_cursor);

        if let NetworkMessage::NetworkPacket(ref mut packet, ..) = deserialized {
            if let NetworkPacketType::DirectMessage(..) = packet.packet_type {
                assert_eq!(packet.network_id, 100);
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
            .peer(self_peer.clone())
            .message_id(NetworkPacket::generate_message_id())
            .network_id(100)
            .message(UCursor::build_from_view(text_msg.clone()))
            .build_broadcast()?;

        let serialized = msg.serialize();
        let s11n_cursor = UCursor::build_from_view(ContainerView::from(serialized));
        let mut deserialized =
            NetworkMessage::deserialize(Some(self_peer.clone()), ipaddr, s11n_cursor);

        if let NetworkMessage::NetworkPacket(ref mut packet, ..) = deserialized {
            if let NetworkPacketType::BroadcastedMessage = packet.packet_type {
                assert_eq!(packet.network_id, 100);
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
    fn req_bannode_test() {
        net_test!(
            NetworkRequest,
            BanNode,
            self_peer(),
            dummy_peer(IpAddr::from([8, 8, 8, 8]), 9999)
        )
    }

    #[test]
    fn req_unbannode_test() {
        net_test!(
            NetworkRequest,
            UnbanNode,
            self_peer(),
            dummy_peer(IpAddr::from([8, 8, 8, 8]), 9999)
        )
    }

    #[test]
    fn req_joinnetwork_test() { net_test!(NetworkRequest, JoinNetwork, self_peer(), 100u16) }

    #[test]
    fn req_leavenetwork_test() { net_test!(NetworkRequest, LeaveNetwork, self_peer(), 100u16) }

    #[test]
    fn resp_invalid_version() {
        let deserialized = NetworkMessage::deserialize(
            None,
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
            None,
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
