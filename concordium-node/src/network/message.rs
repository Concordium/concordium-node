use super::{NetworkPacket, NetworkRequest, NetworkResponse};
use crate::{
    common::{
        get_current_stamp,
        serialization::{Deserializable, ReadArchive, Serializable, WriteArchive},
    },
    failure::Fallible,
    network::{AsProtocolMessageType, ProtocolMessageType, PROTOCOL_NAME, PROTOCOL_VERSION},
};

use std::convert::TryFrom;

#[cfg(feature = "s11n_nom")]
use crate::network::serialization::nom::s11n_network_message;

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
        archive.write(PROTOCOL_NAME.as_bytes())?;
        archive.write_u16(PROTOCOL_VERSION)?;
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
        archive.tag_slice(PROTOCOL_NAME.as_bytes())?;
        archive.tag(PROTOCOL_VERSION)?;

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
        io::{Seek, SeekFrom, Write},
        net::{IpAddr, Ipv4Addr, SocketAddr},
        str::FromStr,
    };

    use super::*;
    use crate::{
        common::{
            serialization::{Deserializable, ReadArchiveAdapter, WriteArchiveAdapter},
            P2PNodeId, P2PPeer, P2PPeerBuilder, PeerType, RemotePeer,
        },
        network::{NetworkId, NetworkPacket, NetworkPacketBuilder, NetworkPacketType},
    };
    use concordium_common::UCursor;

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
        // 1. Generate payload on disk
        let mut payload = UCursor::build_from_temp_file()?;
        let mut pending_content_size = content_size;
        while pending_content_size != 0 {
            let chunk: String = thread_rng()
                .sample_iter(&Alphanumeric)
                .take(std::cmp::min(4096, pending_content_size))
                .collect();
            pending_content_size -= chunk.len();

            payload.write_all(chunk.as_bytes())?;
        }

        payload.seek(SeekFrom::Start(0))?;
        assert_eq!( payload.len(), content_size as u64);
        assert_eq!( payload.position(), 0);

        // 2. Generate packet.
        let p2p_node_id = P2PNodeId::from_str("000000002dd2b6ed")?;
        let pkt = NetworkPacketBuilder::default()
            .peer(P2PPeer::from(
                PeerType::Node,
                p2p_node_id.clone(),
                SocketAddr::new(IpAddr::from_str("127.0.0.1")?, 8888),
            ))
            .message_id(NetworkPacket::generate_message_id())
            .network_id(NetworkId::from(111))
            .message(payload)
            .build_direct(p2p_node_id)?;
        let message = NetworkMessage::NetworkPacket(pkt, Some(get_current_stamp()), None);

        // 3. Serialize package into archive (on disk)
        let archive_cursor = UCursor::build_from_temp_file()?;
        let mut archive = WriteArchiveAdapter::from(archive_cursor);
        message.serialize(&mut archive)?;

        let mut out_cursor = archive.into_inner();
        out_cursor.seek(SeekFrom::Start(0))?;
        Ok(out_cursor)
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

        let mut archive = ReadArchiveAdapter::new(
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
