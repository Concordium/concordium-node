use super::{NetworkPacket, NetworkPacketBuilder, NetworkRequest, NetworkResponse};
use base64;

use crate::{
    common::{get_current_stamp, ConnectionType, ContainerView, P2PNodeId, P2PPeer, UCursor},
    failure::{err_msg, Fallible},
    network::{
        PROTOCOL_MESSAGE_ID_LENGTH, PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE,
        PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE, PROTOCOL_MESSAGE_TYPE_LENGTH,
        PROTOCOL_MESSAGE_TYPE_REQUEST_BANNODE, PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE,
        PROTOCOL_MESSAGE_TYPE_REQUEST_GET_PEERS, PROTOCOL_MESSAGE_TYPE_REQUEST_HANDSHAKE,
        PROTOCOL_MESSAGE_TYPE_REQUEST_JOINNETWORK, PROTOCOL_MESSAGE_TYPE_REQUEST_LEAVENETWORK,
        PROTOCOL_MESSAGE_TYPE_REQUEST_PING, PROTOCOL_MESSAGE_TYPE_REQUEST_UNBANNODE,
        PROTOCOL_MESSAGE_TYPE_RESPONSE_FINDNODE, PROTOCOL_MESSAGE_TYPE_RESPONSE_HANDSHAKE,
        PROTOCOL_MESSAGE_TYPE_RESPONSE_PEERSLIST, PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG,
        PROTOCOL_NAME, PROTOCOL_NETWORK_CONTENT_SIZE_LENGTH, PROTOCOL_NETWORK_ID_LENGTH,
        PROTOCOL_NODE_ID_LENGTH, PROTOCOL_PORT_LENGTH, PROTOCOL_SENT_TIMESTAMP_LENGTH,
        PROTOCOL_VERSION,
    },
};

#[cfg(feature = "s11n_nom")]
use crate::network::serialization::nom::s11n_network_message;

use std::{
    collections::HashSet,
    io::{Seek, SeekFrom},
    net::IpAddr,
    str,
};

const PROTOCOL_PEERS_COUNT_LENGTH: usize = 3;
const PROTOCOL_NETWORK_IDS_COUNT_LENGTH: usize = 5;
const PROTOCOL_HANDSHAKE_CONTENT_LENGTH: usize = 10;

#[derive(Debug, Clone)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkMessage {
    NetworkRequest(NetworkRequest, Option<u64>, Option<u64>),
    NetworkResponse(NetworkResponse, Option<u64>, Option<u64>),
    NetworkPacket(NetworkPacket, Option<u64>, Option<u64>),
    UnknownMessage,
    InvalidMessage,
}

/// It deserializes from `bytes` into a `NetworkPacket::DirectMessage`.
fn deserialize_direct_message(
    peer: P2PPeer,
    timestamp: u64,
    pkt: &mut UCursor,
) -> Fallible<NetworkMessage> {
    let min_packet_size = PROTOCOL_NODE_ID_LENGTH
        + PROTOCOL_MESSAGE_ID_LENGTH
        + PROTOCOL_NETWORK_ID_LENGTH
        + PROTOCOL_NETWORK_CONTENT_SIZE_LENGTH;

    ensure!(
        pkt.len() >= pkt.position() + min_packet_size as u64,
        "Direct Message needs at least {} bytes",
        min_packet_size
    );

    let view = pkt.read_into_view(min_packet_size)?;
    let buf = view.as_slice();

    // 1. Load receiver_id
    let receiver_id = P2PNodeId::from_str(str::from_utf8(&buf[..PROTOCOL_NODE_ID_LENGTH])?)?;
    let buf = &buf[PROTOCOL_NODE_ID_LENGTH..];

    // 2. Load msg_id
    let msgid = String::from_utf8(buf[..PROTOCOL_MESSAGE_ID_LENGTH].to_vec())?;
    let buf = &buf[PROTOCOL_MESSAGE_ID_LENGTH..];

    // 3. Load network_id
    let network_id = str::from_utf8(&buf[..PROTOCOL_NETWORK_ID_LENGTH])?
        .parse::<u16>()
        .unwrap_or(0 as u16);
    let buf = &buf[PROTOCOL_NETWORK_ID_LENGTH..];

    // 4. Load content size and content
    let content_size =
        str::from_utf8(&buf[..PROTOCOL_NETWORK_CONTENT_SIZE_LENGTH])?.parse::<usize>()?;
    // let buf = &buf[PROTOCOL_NETWORK_CONTENT_SIZE_LENGTH..];

    ensure!(
        pkt.len() >= pkt.position() + content_size as u64,
        "Direct Message content needs {} bytes",
        content_size
    );
    let content_cursor = pkt.sub(pkt.position())?;

    let packet = NetworkPacketBuilder::default()
        .peer(peer)
        .message_id(msgid)
        .network_id(network_id)
        .message(content_cursor)
        .build_direct(receiver_id)?;

    Ok(NetworkMessage::NetworkPacket(
        packet,
        Some(timestamp),
        Some(get_current_stamp()),
    ))
}

fn deserialize_broadcast_message(
    peer: P2PPeer,
    timestamp: u64,
    pkt: &mut UCursor,
) -> Fallible<NetworkMessage> {
    let min_packet_size = PROTOCOL_MESSAGE_ID_LENGTH
        + PROTOCOL_NETWORK_ID_LENGTH
        + PROTOCOL_NETWORK_CONTENT_SIZE_LENGTH;

    ensure!(
        pkt.len() >= pkt.position() + min_packet_size as u64,
        "Broadcast Message needs at least {} bytes",
        min_packet_size
    );
    let view = pkt.read_into_view(min_packet_size)?;
    let buf = view.as_slice();

    // 1. Load `message id`
    let message_id = String::from_utf8(buf[..PROTOCOL_MESSAGE_ID_LENGTH].to_vec())?;
    let buf = &buf[PROTOCOL_MESSAGE_ID_LENGTH..];

    let network_id = str::from_utf8(&buf[..PROTOCOL_NETWORK_ID_LENGTH])?.parse::<u16>()?;
    let buf = &buf[PROTOCOL_NETWORK_ID_LENGTH..];

    let content_size =
        str::from_utf8(&buf[..PROTOCOL_NETWORK_CONTENT_SIZE_LENGTH])?.parse::<usize>()?;
    ensure!(
        pkt.len() >= pkt.position() + content_size as u64,
        "Broadcast Message content needs {} bytes",
        content_size
    );

    Ok(NetworkMessage::NetworkPacket(
        NetworkPacketBuilder::default()
            .peer(peer)
            .message_id(message_id)
            .network_id(network_id)
            .message(pkt.sub(pkt.position())?)
            .build_broadcast()?,
        Some(timestamp),
        Some(get_current_stamp()),
    ))
}

/// Deserialize `FindNode` Request message
fn deserialize_request_find_node(
    peer: P2PPeer,
    timestamp: u64,
    pkt: &mut UCursor,
) -> Fallible<NetworkMessage> {
    let min_packet_size = PROTOCOL_NODE_ID_LENGTH;
    ensure!(
        pkt.len() >= pkt.position() + min_packet_size as u64,
        "Find Node Request needs {} bytes",
        min_packet_size
    );

    let view = pkt.read_into_view(PROTOCOL_NODE_ID_LENGTH)?;
    let node_id_str = str::from_utf8(view.as_slice())?;
    let node_id = P2PNodeId::from_str(node_id_str)?;

    Ok(NetworkMessage::NetworkRequest(
        NetworkRequest::FindNode(peer, node_id),
        Some(timestamp),
        Some(get_current_stamp()),
    ))
}

/// Deserialize `Join Network` Request message.
fn deserialize_request_join_network(
    peer: P2PPeer,
    timestamp: u64,
    pkt: &mut UCursor,
) -> Fallible<NetworkMessage> {
    let min_packet_size = PROTOCOL_NETWORK_ID_LENGTH;
    ensure!(
        pkt.len() >= pkt.position() + min_packet_size as u64,
        "Join Network Request needs {} bytes",
        min_packet_size
    );

    let view = pkt.read_into_view(PROTOCOL_NETWORK_ID_LENGTH)?;
    let network_id = str::from_utf8(view.as_slice())?.parse::<u16>()?;

    Ok(NetworkMessage::NetworkRequest(
        NetworkRequest::JoinNetwork(peer, network_id),
        Some(timestamp),
        Some(get_current_stamp()),
    ))
}

/// Deserialize `Leave Network` Request message.
fn deserialize_request_leave_network(
    peer: P2PPeer,
    timestamp: u64,
    pkt: &mut UCursor,
) -> Fallible<NetworkMessage> {
    let min_packet_size = PROTOCOL_NETWORK_ID_LENGTH;
    ensure!(
        pkt.len() >= pkt.position() + min_packet_size as u64,
        "Leave Network Request needs {} bytes",
        min_packet_size
    );

    let view = pkt.read_into_view(PROTOCOL_NETWORK_ID_LENGTH)?;
    let network_id = str::from_utf8(view.as_slice())?.parse::<u16>()?;

    Ok(NetworkMessage::NetworkRequest(
        NetworkRequest::LeaveNetwork(peer, network_id),
        Some(timestamp),
        Some(get_current_stamp()),
    ))
}

fn deserialize_list_of_peers(pkt: &mut UCursor) -> Fallible<Vec<P2PPeer>> {
    let min_packet_size = PROTOCOL_PEERS_COUNT_LENGTH;
    ensure!(
        pkt.len() >= pkt.position() + min_packet_size as u64,
        "List of peers requires at least {} bytes",
        min_packet_size
    );

    let view = pkt.read_into_view(PROTOCOL_PEERS_COUNT_LENGTH)?;
    let buf = view.as_slice();

    let peers_count = str::from_utf8(&buf[..PROTOCOL_PEERS_COUNT_LENGTH])?.parse::<usize>()?;
    let mut peers = Vec::with_capacity(peers_count);

    for _ in 0..peers_count {
        peers.push(P2PPeer::deserialize(pkt)?);
    }

    Ok(peers)
}

fn deserialize_response_find_node(
    sender: P2PPeer,
    timestamp: u64,
    pkt: &mut UCursor,
) -> Fallible<NetworkMessage> {
    let peers = deserialize_list_of_peers(pkt)?;
    Ok(NetworkMessage::NetworkResponse(
        NetworkResponse::FindNode(sender, peers),
        Some(timestamp),
        Some(get_current_stamp()),
    ))
}

fn deserialize_response_peer_list(
    sender: P2PPeer,
    timestamp: u64,
    pkt: &mut UCursor,
) -> Fallible<NetworkMessage> {
    let peers = deserialize_list_of_peers(pkt)?;
    Ok(NetworkMessage::NetworkResponse(
        NetworkResponse::PeerList(sender, peers),
        Some(timestamp),
        Some(get_current_stamp()),
    ))
}

fn deserialize_request_get_peers(
    sender: P2PPeer,
    timestamp: u64,
    pkt: &mut UCursor,
) -> Fallible<NetworkMessage> {
    let min_packet_size = PROTOCOL_NETWORK_IDS_COUNT_LENGTH;
    ensure!(
        pkt.len() >= pkt.position() + min_packet_size as u64,
        "Get Peers Request needs at least {} bytes",
        min_packet_size
    );
    let view = pkt.read_into_view(min_packet_size)?;
    let buf = view.as_slice();

    let network_ids_count =
        str::from_utf8(&buf[..PROTOCOL_NETWORK_IDS_COUNT_LENGTH])?.parse::<usize>()?;
    let min_packet_size = network_ids_count * PROTOCOL_NETWORK_ID_LENGTH;

    ensure!(
        pkt.len() >= pkt.position() + min_packet_size as u64,
        "Get Peers Request needs {} bytes for {} network ids",
        min_packet_size,
        network_ids_count
    );
    let view = pkt.read_into_view(min_packet_size)?;
    let buf = view.as_slice();

    let mut networks = HashSet::with_capacity(network_ids_count);
    let mut offset = 0;
    for _ in 0..network_ids_count {
        let network_id =
            str::from_utf8(&buf[offset..][..PROTOCOL_NETWORK_ID_LENGTH])?.parse::<u16>()?;
        networks.insert(network_id);
        offset += PROTOCOL_NETWORK_ID_LENGTH;
    }

    Ok(NetworkMessage::NetworkRequest(
        NetworkRequest::GetPeers(sender, networks),
        Some(timestamp),
        Some(get_current_stamp()),
    ))
}

/// # TODO
/// Try to remove handshake content to avoid copied vector from `pkt`. Use the
/// packet directly
fn deserialize_common_handshake(
    pkt: &mut UCursor,
) -> Fallible<(P2PNodeId, u16, HashSet<u16>, ContainerView)> {
    let min_packet_size =
        PROTOCOL_NODE_ID_LENGTH + PROTOCOL_PORT_LENGTH + PROTOCOL_NETWORK_IDS_COUNT_LENGTH;

    ensure!(
        pkt.len() >= pkt.position() + min_packet_size as u64,
        "Handshare requires at least {} bytes",
        min_packet_size
    );
    let view = pkt.read_into_view(min_packet_size)?;
    let buf = view.as_slice();

    let node_id = P2PNodeId::from_str(str::from_utf8(&buf[..PROTOCOL_NODE_ID_LENGTH])?)?;
    let buf = &buf[PROTOCOL_NODE_ID_LENGTH..];

    let port = str::from_utf8(&buf[..PROTOCOL_PORT_LENGTH])?.parse::<u16>()?;
    let buf = &buf[PROTOCOL_PORT_LENGTH..];

    // Read network ids count
    let network_ids_count =
        str::from_utf8(&buf[..PROTOCOL_NETWORK_IDS_COUNT_LENGTH])?.parse::<usize>()?;
    let min_packet_size =
        network_ids_count * PROTOCOL_NETWORK_ID_LENGTH + PROTOCOL_HANDSHAKE_CONTENT_LENGTH;
    ensure!(
        pkt.len() >= pkt.position() + min_packet_size as u64,
        "Handshare requires {} bytes for network ids",
        min_packet_size
    );

    let view = pkt.read_into_view(min_packet_size)?;
    let buf = view.as_slice();

    let mut network_ids = HashSet::with_capacity(network_ids_count);
    let mut buf_offset = 0;
    for _ in 0..network_ids_count {
        let nid =
            str::from_utf8(&buf[buf_offset..][..PROTOCOL_NETWORK_ID_LENGTH])?.parse::<u16>()?;
        network_ids.insert(nid);
        buf_offset += PROTOCOL_NETWORK_ID_LENGTH;
    }
    let buf = &buf[buf_offset..];

    let content_size =
        str::from_utf8(&buf[..PROTOCOL_HANDSHAKE_CONTENT_LENGTH])?.parse::<usize>()?;
    ensure!(
        pkt.len() >= pkt.position() + content_size as u64,
        "Handshare requires {} bytes for its content",
        min_packet_size
    );

    let content = pkt.read_into_view(content_size)?;
    Ok((node_id, port, network_ids, content))
}

fn deserialize_response_handshake(
    ip: IpAddr,
    timestamp: u64,
    pkt: &mut UCursor,
) -> Fallible<NetworkMessage> {
    let (node_id, port, network_ids, content) = deserialize_common_handshake(pkt)?;
    let peer = P2PPeer::from(ConnectionType::Node, node_id, ip, port);

    Ok(NetworkMessage::NetworkResponse(
        NetworkResponse::Handshake(peer, network_ids, content.as_slice().to_vec()),
        Some(timestamp),
        Some(get_current_stamp()),
    ))
}

fn deserialize_request_handshake(
    ip: IpAddr,
    timestamp: u64,
    pkt: &mut UCursor,
) -> Fallible<NetworkMessage> {
    let (node_id, port, network_ids, content) = deserialize_common_handshake(pkt)?;
    let peer = P2PPeer::from(ConnectionType::Node, node_id, ip, port);

    Ok(NetworkMessage::NetworkRequest(
        NetworkRequest::Handshake(peer, network_ids, content.as_slice().to_vec()),
        Some(timestamp),
        Some(get_current_stamp()),
    ))
}

impl NetworkMessage {
    #[cfg(feature = "s11n_nom")]
    pub fn deserialize_nom(bytes: &[u8]) -> NetworkMessage {
        match s11n_network_message(bytes) {
            Ok(msg) => msg.1,
            Err(e) => {
                warn!("Network deserialization has failed {}", e);
                NetworkMessage::InvalidMessage
            }
        }
    }

    pub fn try_deserialize(
        peer: Option<P2PPeer>,
        ip: IpAddr,
        mut pkt: UCursor,
    ) -> Fallible<NetworkMessage> {
        let protocol_name_length = PROTOCOL_NAME.len();
        let protocol_version_length = PROTOCOL_VERSION.len();

        pkt.seek(SeekFrom::Start(0))?;

        // Load header into mem.
        let header_size = protocol_name_length
            + protocol_version_length
            + PROTOCOL_SENT_TIMESTAMP_LENGTH
            + PROTOCOL_MESSAGE_TYPE_LENGTH;

        ensure!(
            pkt.len() >= header_size as u64,
            "Network Message requires at least {} bytes",
            header_size
        );
        let view = pkt.read_into_view(header_size)?;
        let header = view.as_slice();

        // Check protocol name
        let protocol = unsafe { str::from_utf8_unchecked(&header[..protocol_name_length]) };
        ensure!(protocol == PROTOCOL_NAME, "Unsupported protocol");
        let header = &header[protocol_name_length..];

        // Check version
        let version = unsafe { str::from_utf8_unchecked(&header[..protocol_version_length]) };
        ensure!(version == PROTOCOL_VERSION, "Unsupported version");
        let header = &header[protocol_version_length..];

        // Load timestamp
        let timestamp = {
            let timestamp = base64::decode(&header[..PROTOCOL_SENT_TIMESTAMP_LENGTH])?;
            if timestamp.len() != 8 {
                error!("Invalid timesptamp {:?}", timestamp);
            }
            debug_assert_eq!(timestamp.len(), 8);
            let mut a = [0; 8];
            a.copy_from_slice(timestamp.as_slice());
            u64::from_le_bytes(a)
        };
        let header = &header[PROTOCOL_SENT_TIMESTAMP_LENGTH..];

        // Load message type id
        // **ATTENTION**
        // It is not an `str`, just a byte slice. Do not use as `str` because it did not
        // be checked as valid utf8.
        // This is unsafe code is a performance optimization.
        let message_type_id =
            unsafe { str::from_utf8_unchecked(&header[..PROTOCOL_MESSAGE_TYPE_LENGTH]) };
        match message_type_id {
            PROTOCOL_MESSAGE_TYPE_REQUEST_PING => Ok(NetworkMessage::NetworkRequest(
                NetworkRequest::Ping(
                    peer.ok_or_else(|| err_msg("Ping message requires a valid peer"))?,
                ),
                Some(timestamp),
                Some(get_current_stamp()),
            )),
            PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG => Ok(NetworkMessage::NetworkResponse(
                NetworkResponse::Pong(
                    peer.ok_or_else(|| err_msg("Pong message requires a valid peer"))?,
                ),
                Some(timestamp),
                Some(get_current_stamp()),
            )),
            PROTOCOL_MESSAGE_TYPE_RESPONSE_HANDSHAKE => {
                deserialize_response_handshake(ip, timestamp, &mut pkt)
            }
            PROTOCOL_MESSAGE_TYPE_REQUEST_GET_PEERS => deserialize_request_get_peers(
                peer.ok_or_else(|| err_msg("FindNode Request requires a valid peer"))?,
                timestamp,
                &mut pkt,
            ),
            PROTOCOL_MESSAGE_TYPE_REQUEST_HANDSHAKE => {
                deserialize_request_handshake(ip, timestamp, &mut pkt)
            }
            PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE => deserialize_request_find_node(
                peer.ok_or_else(|| err_msg("FindNode Request requires a valid peer"))?,
                timestamp,
                &mut pkt,
            ),
            PROTOCOL_MESSAGE_TYPE_REQUEST_BANNODE => Ok(NetworkMessage::NetworkRequest(
                NetworkRequest::BanNode(
                    peer.ok_or_else(|| err_msg("BanNode Request requires a valid peer"))?,
                    P2PPeer::deserialize(&mut pkt)?,
                ),
                Some(timestamp),
                Some(get_current_stamp()),
            )),
            PROTOCOL_MESSAGE_TYPE_REQUEST_UNBANNODE => Ok(NetworkMessage::NetworkRequest(
                NetworkRequest::UnbanNode(
                    peer.ok_or_else(|| err_msg("UnbanNode Request requires a valid peer"))?,
                    P2PPeer::deserialize(&mut pkt)?,
                ),
                Some(timestamp),
                Some(get_current_stamp()),
            )),
            PROTOCOL_MESSAGE_TYPE_REQUEST_JOINNETWORK => deserialize_request_join_network(
                peer.ok_or_else(|| err_msg("Join Network Request requires a valid peer"))?,
                timestamp,
                &mut pkt,
            ),
            PROTOCOL_MESSAGE_TYPE_REQUEST_LEAVENETWORK => deserialize_request_leave_network(
                peer.ok_or_else(|| err_msg("Leave Network Request requires a valid peer"))?,
                timestamp,
                &mut pkt,
            ),
            PROTOCOL_MESSAGE_TYPE_RESPONSE_FINDNODE => deserialize_response_find_node(
                peer.ok_or_else(|| err_msg("Find Node Response requires a valid peer"))?,
                timestamp,
                &mut pkt,
            ),
            PROTOCOL_MESSAGE_TYPE_RESPONSE_PEERSLIST => deserialize_response_peer_list(
                peer.ok_or_else(|| err_msg("Peer List Response requires a valid peer"))?,
                timestamp,
                &mut pkt,
            ),
            PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE => deserialize_direct_message(
                peer.ok_or_else(|| err_msg("Direct Message requires a valid peer"))?,
                timestamp,
                &mut pkt,
            ),
            PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE => deserialize_broadcast_message(
                peer.ok_or_else(|| err_msg("Broadcast Message requires a valid peer"))?,
                timestamp,
                &mut pkt,
            ),
            _ => bail!("Unsupported protocol message"),
        }
    }

    pub fn deserialize(
        connection_peer: Option<P2PPeer>,
        ip: IpAddr,
        pkt: UCursor,
    ) -> NetworkMessage {
        match NetworkMessage::try_deserialize(connection_peer, ip, pkt) {
            Ok(message) => message,
            Err(e) => {
                error!("{:?}", e);
                NetworkMessage::InvalidMessage
            }
        }
    }
}

/// This implementation ignores the reception time stamp.
impl PartialEq for NetworkMessage {
    fn eq(&self, other: &NetworkMessage) -> bool {
        match (self, other) {
            (
                NetworkMessage::NetworkRequest(ref self_nr, self_tm, _),
                NetworkMessage::NetworkRequest(ref other_nr, other_tm, _),
            ) => self_nr == other_nr && self_tm == other_tm,
            (
                NetworkMessage::NetworkResponse(ref self_nr, self_tm, _),
                NetworkMessage::NetworkResponse(ref other_nr, other_tm, _),
            ) => self_nr == other_nr && self_tm == other_tm,
            (
                NetworkMessage::NetworkPacket(ref self_np, self_tm, _),
                NetworkMessage::NetworkPacket(ref other_np, other_tm, _),
            ) => self_np == other_np && self_tm == other_tm,
            (NetworkMessage::UnknownMessage, NetworkMessage::UnknownMessage)
            | (NetworkMessage::InvalidMessage, NetworkMessage::InvalidMessage) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod unit_test {
    use failure::Fallible;
    use rand::{distributions::Alphanumeric, thread_rng, Rng};
    use std::{
        io::Write,
        net::{IpAddr, Ipv4Addr},
        str::FromStr,
    };

    use super::NetworkMessage;
    use crate::{
        common::{ConnectionType, P2PNodeId, P2PPeer, P2PPeerBuilder, UCursor},
        network::{NetworkPacket, NetworkPacketBuilder, NetworkPacketType},
    };

    #[test]
    fn ut_s11n_001_direct_message_from_disk_16m() -> Fallible<()> {
        ut_s11n_001_direct_message_from_disk(16 * 1024 * 1024)
    }

    #[test]
    fn ut_s11n_001_direct_message_from_disk_128m() -> Fallible<()> {
        ut_s11n_001_direct_message_from_disk(128 * 1024 * 1024)
    }

    #[test]
    #[ignore]
    fn ut_s11n_001_direct_message_from_disk_512m() -> Fallible<()> {
        ut_s11n_001_direct_message_from_disk(512 * 1024 * 1024)
    }

    fn make_direct_message_into_disk(content_size: usize) -> Fallible<UCursor> {
        // Create header
        let header = {
            let p2p_node_id = P2PNodeId::from_str("000000002dd2b6ed")?;
            let pkt = NetworkPacketBuilder::default()
                .peer(P2PPeer::from(
                    ConnectionType::Node,
                    p2p_node_id,
                    IpAddr::from_str("127.0.0.1")?,
                    8888,
                ))
                .message_id(NetworkPacket::generate_message_id())
                .network_id(111)
                .message(UCursor::from(vec![]))
                .build_direct(P2PNodeId::from_str("100000002dd2b6ed")?)?;

            let mut h = pkt.serialize();

            // chop the last 10 bytes which are the length of the message
            h.truncate(h.len() - 10);
            h
        };

        // Write header and content size
        let mut cursor = UCursor::build_from_temp_file()?;
        cursor.write_all(header.as_slice())?;
        cursor.write_all(format!("{:010}", content_size).as_bytes())?;

        // Write content
        let mut pending_content_size = content_size;
        while pending_content_size != 0 {
            let chunk: String = thread_rng()
                .sample_iter(&Alphanumeric)
                .take(std::cmp::min(4096, pending_content_size))
                .collect();
            pending_content_size -= chunk.len();

            cursor.write_all(chunk.as_bytes())?;
        }

        assert_eq!(cursor.len(), (content_size + 10 + header.len()) as u64);
        Ok(cursor)
    }

    fn ut_s11n_001_direct_message_from_disk(content_size: usize) -> Fallible<()> {
        // Create serialization data in memory and then move to disk
        let cursor_on_disk = make_direct_message_into_disk(content_size)?;

        // Local stuff
        let local_ip = IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1));
        let local_peer = P2PPeerBuilder::default()
            .connection_type(ConnectionType::Node)
            .ip(local_ip)
            .port(8888)
            .build()?;

        let message =
            NetworkMessage::try_deserialize(Some(local_peer.clone()), local_ip, cursor_on_disk)?;

        if let NetworkMessage::NetworkPacket(ref packet, ..) = message {
            if let NetworkPacketType::DirectMessage(..) = packet.packet_type {
                assert_eq!(packet.peer, local_peer);
                assert_eq!(packet.network_id, 111);
                assert_eq!(packet.message.len(), content_size as u64);
            } else {
                bail!("Unexpected Packet type");
            }
        } else {
            bail!("Unexpected network message");
        }

        Ok(())
    }
}
