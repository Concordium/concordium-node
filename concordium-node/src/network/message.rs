use super::{NetworkPacket, NetworkRequest, NetworkResponse};
use crate::{
    common::{
        get_current_stamp,
        serialization::{Deserializable, ReadArchive, Serializable, WriteArchive},
    },
    failure::Fallible,
    network::{AsProtocolMessageType, ProtocolMessageType, PROTOCOL_NAME, PROTOCOL_VERSION_2},
};
use std::convert::TryFrom;

#[cfg(feature = "s11n_nom")]
use crate::network::serialization::nom::s11n_network_message;

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

impl AsProtocolMessageType for NetworkMessage {
    fn protocol_type(&self) -> ProtocolMessageType {
        match self {
            NetworkMessage::NetworkRequest(ref request, ..) => request.protocol_type(),
            NetworkMessage::NetworkResponse(ref response, ..) => response.protocol_type(),
            NetworkMessage::NetworkPacket(ref packet, ..) => packet.protocol_type(),
            NetworkMessage::UnknownMessage | NetworkMessage::InvalidMessage => {
                panic!("Invalid or Unknown messages are not serializable")
            }
        }
    }
}

impl Serializable for NetworkMessage {
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        archive.write_str(PROTOCOL_NAME)?;
        archive.write_u16(PROTOCOL_VERSION_2)?;
        archive.write_u64(get_current_stamp as u64)?;
        archive.write_u8(self.protocol_type() as u8)?;
        match self {
            NetworkMessage::NetworkRequest(ref request, ..) => request.serialize(archive),
            NetworkMessage::NetworkResponse(ref response, ..) => response.serialize(archive),
            NetworkMessage::NetworkPacket(ref packet, ..) => packet.serialize(archive),
            NetworkMessage::UnknownMessage | NetworkMessage::InvalidMessage => {
                bail!("Unsupported type of NetworkMessage")
            }
        }
    }
}

impl Deserializable for NetworkMessage {
    fn deserialize<A>(archive: &mut A) -> Fallible<NetworkMessage>
    where
        A: ReadArchive, {
        archive.tag_str(PROTOCOL_NAME)?;
        archive.read_u16().and_then(|version| {
            if version == PROTOCOL_VERSION_2 {
                Ok(())
            } else {
                bail!("Incompatible protocol version")
            }
        })?;
        let timestamp: u64 = archive.read_u64()?;
        let protocol_type: ProtocolMessageType = ProtocolMessageType::try_from(archive.read_u8()?)?;
        let message = match protocol_type {
            ProtocolMessageType::RequestPing
            | ProtocolMessageType::RequestFindNode
            | ProtocolMessageType::RequestHandshake
            | ProtocolMessageType::RequestGetPeers
            | ProtocolMessageType::RequestBanNode
            | ProtocolMessageType::RequestUnbanNode
            | ProtocolMessageType::RequestJoinNetwork
            | ProtocolMessageType::RequestLeaveNetwork
            | ProtocolMessageType::RequestRetransmit => {
                let request = NetworkRequest::deserialize(archive)?;
                NetworkMessage::NetworkRequest(request, Some(timestamp), Some(get_current_stamp()))
            }
            ProtocolMessageType::ResponsePong
            | ProtocolMessageType::ResponseFindNode
            | ProtocolMessageType::ResponsePeersList
            | ProtocolMessageType::ResponseHandshake => {
                let response = NetworkResponse::deserialize(archive)?;
                NetworkMessage::NetworkResponse(
                    response,
                    Some(timestamp),
                    Some(get_current_stamp()),
                )
            }
            ProtocolMessageType::DirectMessage | ProtocolMessageType::BroadcastedMessage => {
                let packet = NetworkPacket::deserialize(archive)?;
                NetworkMessage::NetworkPacket(packet, Some(timestamp), Some(get_current_stamp()))
            }
        };

        Ok(message)
    }
}

#[cfg(test)]
mod unit_test {
    use failure::Fallible;
    use rand::{distributions::Alphanumeric, thread_rng, Rng};
    use std::{
        io::Write,
        net::{IpAddr, Ipv4Addr, SocketAddr},
        str::FromStr,
    };

    use super::*;
    use crate::{
        common::{
            serialization::{Deserializable, IOReadArchiveAdapter},
            P2PNodeId, P2PPeer, P2PPeerBuilder, PeerType, RemotePeer, UCursor,
        },
        network::{NetworkId, NetworkPacket, NetworkPacketBuilder, NetworkPacketType},
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
                    PeerType::Node,
                    p2p_node_id,
                    SocketAddr::new(IpAddr::from_str("127.0.0.1")?, 8888),
                ))
                .message_id(NetworkPacket::generate_message_id())
                .network_id(NetworkId::from(111))
                .message(Box::new(UCursor::from(vec![])))
                .build_direct(P2PNodeId::from_str("100000002dd2b6ed")?)?;

            let mut h = serialize_into_memory!(pkt)?;

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
            .peer_type(PeerType::Node)
            .addr(SocketAddr::new(local_ip, 8888))
            .build()?;

        let mut archive = IOReadArchiveAdapter::new(
            cursor_on_disk,
            RemotePeer::PostHandshake(local_peer.clone()),
            local_ip,
        );
        let message = NetworkMessage::deserialize(&mut archive)?;

        if let NetworkMessage::NetworkPacket(ref packet, ..) = message {
            if let NetworkPacketType::DirectMessage(..) = packet.packet_type {
                assert_eq!(packet.peer, local_peer);
                assert_eq!(packet.network_id, NetworkId::from(111));
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
