use byteorder::{ReadBytesExt, WriteBytesExt};
use failure::Fallible;

use super::{
    AsProtocolMessageType, NetworkPacket, NetworkRequest, NetworkResponse, ProtocolMessageType,
    PROTOCOL_NAME, PROTOCOL_VERSION,
};
use crate::common::get_current_stamp;
use concordium_common::serial::{NoParam, Serial};

use std::{convert::TryFrom, ops::Deref};

pub const NETWORK_MESSAGE_PROTOCOL_TYPE_IDX: usize = 4 + // PROTOCOL_NAME.len()
    1 + // PROTOCOL_VERSION
    8; // Timestamp: get_current_stamp

#[derive(Debug)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct NetworkMessage {
    pub timestamp1: Option<u64>,
    pub timestamp2: Option<u64>,
    pub payload:    NetworkMessagePayload,
}

#[derive(Debug)]
pub enum NetworkMessagePayload {
    NetworkRequest(NetworkRequest),
    NetworkResponse(NetworkResponse),
    NetworkPacket(NetworkPacket),
}

impl AsProtocolMessageType for NetworkMessage {
    fn protocol_message_type(&self) -> ProtocolMessageType {
        match self.payload {
            NetworkMessagePayload::NetworkRequest(..) => ProtocolMessageType::Request,
            NetworkMessagePayload::NetworkResponse(..) => ProtocolMessageType::Response,
            NetworkMessagePayload::NetworkPacket(..) => ProtocolMessageType::Packet,
        }
    }
}

impl Serial for NetworkMessage {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        // verify the protocol name and version
        let mut protocol_name = vec![0u8; PROTOCOL_NAME.len()];
        source.read_exact(&mut protocol_name)?;
        if protocol_name.deref() != PROTOCOL_NAME.as_bytes() {
            bail!("Unknown protocol name (`{:?}`)! ", protocol_name.deref())
        }
        let protocol_version = u8::deserial(source)?;
        if protocol_version != PROTOCOL_VERSION {
            bail!("Unknown protocol version (`{:?}`)", protocol_version)
        }

        let timestamp = u64::deserial(source)?;
        let protocol_type: ProtocolMessageType = ProtocolMessageType::try_from(source.read_u8()?)?;
        let message = match protocol_type {
            ProtocolMessageType::Request => {
                let request = NetworkRequest::deserial(source)?;
                NetworkMessage {
                    timestamp1: Some(timestamp),
                    timestamp2: Some(get_current_stamp()),
                    payload:    NetworkMessagePayload::NetworkRequest(request),
                }
            }
            ProtocolMessageType::Response => {
                let response = NetworkResponse::deserial(source)?;
                NetworkMessage {
                    timestamp1: Some(timestamp),
                    timestamp2: Some(get_current_stamp()),
                    payload:    NetworkMessagePayload::NetworkResponse(response),
                }
            }
            ProtocolMessageType::Packet => {
                let packet = NetworkPacket::deserial(source)?;
                NetworkMessage {
                    timestamp1: Some(timestamp),
                    timestamp2: Some(get_current_stamp()),
                    payload:    NetworkMessagePayload::NetworkPacket(packet),
                }
            }
        };

        Ok(message)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_all(PROTOCOL_NAME.as_bytes())?;
        PROTOCOL_VERSION.serial(target)?;
        get_current_stamp().serial(target)?;
        (self.protocol_message_type() as u8).serial(target)?;

        match self.payload {
            NetworkMessagePayload::NetworkRequest(ref request) => request.serial(target),
            NetworkMessagePayload::NetworkResponse(ref response) => response.serial(target),
            NetworkMessagePayload::NetworkPacket(ref packet) => packet.serial(target),
        }
    }
}

#[cfg(test)]
mod unit_test {
    use failure::Fallible;
    use rand::{distributions::Alphanumeric, thread_rng, Rng};
    use std::{
        io::{Seek, SeekFrom, Write},
        str::FromStr,
    };

    use super::*;
    use crate::{
        common::P2PNodeId,
        network::{NetworkId, NetworkPacket, NetworkPacketType},
    };
    use concordium_common::{hybrid_buf::HybridBuf, serial::Serial};

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
        let pkt = NetworkPacket {
            packet_type: NetworkPacketType::DirectMessage(p2p_node_id),
            network_id:  NetworkId::from(111),
            message:     payload,
        };
        let message = NetworkMessage {
            timestamp1: Some(get_current_stamp()),
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkPacket(pkt),
        };

        // 3. Serialize package into a file
        let mut buffer = HybridBuf::new_on_disk()?;
        message.serial(&mut buffer)?;

        buffer.seek(SeekFrom::Start(0))?;
        Ok(buffer)
    }

    fn ut_s11n_001_direct_message_from_disk(content_size: usize) -> Fallible<()> {
        // Create serialization data in memory and then move to disk
        let mut buffer = make_direct_message_into_disk(content_size)?;

        let mut message = NetworkMessage::deserial(&mut buffer)?;

        if let NetworkMessagePayload::NetworkPacket(ref mut packet) = message.payload {
            if let NetworkPacketType::BroadcastedMessage(..) = packet.packet_type {
                bail!("Unexpected Packet type");
            }
            assert_eq!(packet.network_id, NetworkId::from(111));
            assert_eq!(packet.message.clone().remaining_len()?, content_size as u64);
        } else {
            bail!("Unexpected network message");
        }

        Ok(())
    }
}
