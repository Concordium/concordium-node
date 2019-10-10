use crate::network::NetworkMessage;
use serde_cbor::from_reader;
use std::io::Read;

pub fn s11n_network_message<T: Read>(input: &mut T) -> NetworkMessage {
    match from_reader::<NetworkMessage, &mut T>(input) {
        Ok(nm) => nm,
        Err(e) => panic!("{}", e),
    }
}

#[cfg(test)]
mod unit_test {
    use serde_cbor::ser;
    use std::convert::TryFrom;

    use super::s11n_network_message;
    use concordium_common::hybrid_buf::HybridBuf;

    use crate::{
        common::P2PNodeId,
        network::{
            NetworkId, NetworkMessage, NetworkMessagePayload, NetworkPacket, NetworkPacketType,
            NetworkRequest, NetworkResponse,
        },
    };

    fn ut_s11n_001_data() -> Vec<(Vec<u8>, NetworkMessage)> {
        let mut direct_message_content = HybridBuf::try_from(b"Hello world!".to_vec()).unwrap();
        direct_message_content.rewind().unwrap();
        let messages = vec![
            NetworkMessage {
                timestamp1: Some(0),
                timestamp2: None,
                payload:    NetworkMessagePayload::NetworkRequest(NetworkRequest::Ping),
            },
            NetworkMessage {
                timestamp1: Some(11529215046068469760),
                timestamp2: None,
                payload:    NetworkMessagePayload::NetworkRequest(NetworkRequest::Ping),
            },
            NetworkMessage {
                timestamp1: Some(u64::max_value()),
                timestamp2: None,
                payload:    NetworkMessagePayload::NetworkResponse(NetworkResponse::Pong),
            },
            NetworkMessage {
                timestamp1: Some(10),
                timestamp2: None,
                payload:    NetworkMessagePayload::NetworkPacket(NetworkPacket {
                    packet_type: NetworkPacketType::DirectMessage(P2PNodeId::default()),
                    network_id:  NetworkId::from(100u16),
                    message:     direct_message_content,
                }),
            },
        ];

        let mut messages_data: Vec<(Vec<u8>, NetworkMessage)> = Vec::with_capacity(messages.len());
        for message in messages {
            let data: Vec<u8> = ser::to_vec(&message).unwrap();
            messages_data.push((data, message));
        }

        messages_data
    }

    #[test]
    fn ut_s11n_001() {
        let messages = ut_s11n_001_data();
        for (cbor, expected) in messages {
            let output = s11n_network_message(&mut std::io::Cursor::new(cbor));
            assert_eq!(format!("{:?}", output), format!("{:?}", expected));
        }
    }
}
