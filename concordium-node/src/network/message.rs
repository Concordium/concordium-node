use super::{
    AsProtocolMessageType, NetworkPacket, NetworkRequest, NetworkResponse, ProtocolMessageType,
};

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct NetworkMessage {
    pub timestamp1: Option<u64>,
    pub timestamp2: Option<u64>,
    pub payload:    NetworkMessagePayload,
}

impl NetworkMessage {
    // this function is for benchmark purposes only
    pub fn rewind_packet(&mut self) {
        if let NetworkMessagePayload::NetworkPacket(ref mut packet) = self.payload {
            packet.message.rewind().unwrap()
        } else {
            unreachable!();
        }
    }
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
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
        common::{get_current_stamp, P2PNodeId},
        network::{NetworkId, NetworkPacket, NetworkPacketType},
    };
    use concordium_common::hybrid_buf::HybridBuf;

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
        let mut message = NetworkMessage {
            timestamp1: Some(get_current_stamp()),
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkPacket(pkt),
        };

        // 3. Serialize package into a file
        let mut buffer = HybridBuf::new_on_disk()?;
        message.serialize(&mut buffer)?;

        buffer.seek(SeekFrom::Start(0))?;
        Ok(buffer)
    }

    fn ut_s11n_001_direct_message_from_disk(content_size: usize) -> Fallible<()> {
        // Create serialization data in memory and then move to disk
        let mut buffer = make_direct_message_into_disk(content_size)?;

        let mut message = NetworkMessage::deserialize(&buffer.remaining_bytes()?)?;

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
