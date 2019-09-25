use byteorder::{ReadBytesExt, WriteBytesExt};
use failure::Fallible;

use super::{NetworkPacket, NetworkRequest, NetworkResponse};
use crate::{
    common::get_current_stamp,
    network::{AsProtocolMessageType, ProtocolMessageType, PROTOCOL_NAME, PROTOCOL_VERSION},
};
use concordium_common::Serial;

use std::{convert::TryFrom, ops::Deref, sync::Arc};

pub const NETWORK_MESSAGE_PROTOCOL_TYPE_IDX: usize = 13 +    // PROTOCOL_NAME.len()
    2 +     // PROTOCOL_VERSION
    8; // Timestamp: get_current_stamp

#[cfg(feature = "s11n_nom")]
use crate::network::serialization::nom::s11n_network_message;

#[derive(Debug, Clone)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkMessage {
    NetworkRequest(NetworkRequest, Option<u64>, Option<u64>),
    NetworkResponse(NetworkResponse, Option<u64>, Option<u64>),
    NetworkPacket(Arc<NetworkPacket>, Option<u64>, Option<u64>),
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

impl AsProtocolMessageType for NetworkMessage {
    fn protocol_message_type(&self) -> ProtocolMessageType {
        match self {
            NetworkMessage::NetworkRequest(..) => ProtocolMessageType::Request,
            NetworkMessage::NetworkResponse(..) => ProtocolMessageType::Response,
            NetworkMessage::NetworkPacket(..) => ProtocolMessageType::Packet,
            NetworkMessage::InvalidMessage => unreachable!("Invalid messages are not serializable"),
        }
    }
}

impl Serial for NetworkMessage {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        // verify the protocol name and version
        let mut protocol_name = vec![0u8; PROTOCOL_NAME.len()];
        source.read_exact(&mut protocol_name)?;
        if protocol_name.deref() != PROTOCOL_NAME.as_bytes() {
            bail!("Unknown protocol name (`{:?}`)! ", protocol_name.deref())
        }
        let protocol_version = u16::deserial(source)?;
        if protocol_version != PROTOCOL_VERSION {
            bail!("Unknown protocol version (`{:?}`)", protocol_version)
        }

        let timestamp = u64::deserial(source)?;
        let protocol_type: ProtocolMessageType = ProtocolMessageType::try_from(source.read_u8()?)?;
        let message = match protocol_type {
            ProtocolMessageType::Request => {
                let request = NetworkRequest::deserial(source)?;
                NetworkMessage::NetworkRequest(request, Some(timestamp), Some(get_current_stamp()))
            }
            ProtocolMessageType::Response => {
                let response = NetworkResponse::deserial(source)?;
                NetworkMessage::NetworkResponse(
                    response,
                    Some(timestamp),
                    Some(get_current_stamp()),
                )
            }
            ProtocolMessageType::Packet => {
                let packet = Arc::new(NetworkPacket::deserial(source)?);
                NetworkMessage::NetworkPacket(packet, Some(timestamp), Some(get_current_stamp()))
            }
        };

        Ok(message)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_all(PROTOCOL_NAME.as_bytes())?;
        PROTOCOL_VERSION.serial(target)?;
        get_current_stamp().serial(target)?;
        (self.protocol_message_type() as u8).serial(target)?;
        #[cfg(test)]
        const_assert!(network_message_protocol_type; NETWORK_MESSAGE_PROTOCOL_TYPE_IDX == 13 + 2 + 8);

        match self {
            NetworkMessage::NetworkRequest(ref request, ..) => request.serial(target),
            NetworkMessage::NetworkResponse(ref response, ..) => response.serial(target),
            NetworkMessage::NetworkPacket(ref packet, ..) => packet.serial(target),
            NetworkMessage::InvalidMessage => bail!("Unsupported type of NetworkMessage"),
        }
    }
}

#[cfg(test)]
mod unit_test {
    use failure::Fallible;
    use rand::{distributions::Alphanumeric, thread_rng, Rng};
    use std::{
        io::{Seek, SeekFrom, Write},
        net::{IpAddr, SocketAddr},
        str::FromStr,
    };

    use super::*;
    use crate::{
        common::{P2PNodeId, P2PPeer, PeerType},
        network::{NetworkId, NetworkPacket, NetworkPacketType},
    };
    use concordium_common::{hybrid_buf::HybridBuf, Serial};

    #[test]
    fn ut_s11n_001_direct_message_from_disk_16m() -> Fallible<()> {
        ut_s11n_001_direct_message_from_disk(16 * 1024 * 1024)
    }

    #[test]
    #[ignore]
    fn ut_s11n_001_direct_message_from_disk_128m() -> Fallible<()> {
        ut_s11n_001_direct_message_from_disk(128 * 1024 * 1024)
    }

    #[test]
    #[ignore]
    fn ut_s11n_001_direct_message_from_disk_512m() -> Fallible<()> {
        ut_s11n_001_direct_message_from_disk(512 * 1024 * 1024)
    }

    fn make_direct_message_into_disk(content_size: usize) -> Fallible<HybridBuf> {
        // 1. Generate payload on disk
        let mut payload = HybridBuf::new_on_disk()?;
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
        assert_eq!(payload.len()?, content_size as u64);
        assert_eq!(payload.position()?, 0);

        // 2. Generate packet.
        let p2p_node_id = P2PNodeId::from_str("000000002dd2b6ed")?;
        let peer = P2PPeer::from(
            PeerType::Node,
            p2p_node_id.clone(),
            SocketAddr::new(IpAddr::from_str("127.0.0.1")?, 8888),
        );
        let pkt = NetworkPacket {
            packet_type: NetworkPacketType::DirectMessage(p2p_node_id),
            peer,
            network_id: NetworkId::from(111),
            message: payload,
        };
        let message = NetworkMessage::NetworkPacket(Arc::new(pkt), Some(get_current_stamp()), None);

        // 3. Serialize package into a file
        let mut buffer = HybridBuf::new_on_disk()?;
        message.serial(&mut buffer)?;

        buffer.seek(SeekFrom::Start(0))?;
        Ok(buffer)
    }

    fn ut_s11n_001_direct_message_from_disk(content_size: usize) -> Fallible<()> {
        // Create serialization data in memory and then move to disk
        let mut buffer = make_direct_message_into_disk(content_size)?;

        let local_peer = P2PPeer::from(
            PeerType::Node,
            P2PNodeId::from_str("000000002dd2b6ed")?,
            SocketAddr::new(IpAddr::from_str("127.0.0.1")?, 8888),
        );

        let mut message = NetworkMessage::deserial(&mut buffer)?;

        if let NetworkMessage::NetworkPacket(ref mut packet, ..) = message {
            if let NetworkPacketType::BroadcastedMessage(..) = packet.packet_type {
                bail!("Unexpected Packet type");
            }
            assert_eq!(packet.peer, local_peer);
            assert_eq!(packet.network_id, NetworkId::from(111));
            assert_eq!(packet.message.clone().remaining_len()?, content_size as u64);
        } else {
            bail!("Unexpected network message");
        }

        Ok(())
    }
}
