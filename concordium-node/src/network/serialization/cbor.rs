use crate::network::NetworkMessage;
use serde_cbor::from_slice;

pub fn s11n_network_message(input: &[u8]) -> NetworkMessage {
    match from_slice::<NetworkMessage>(input) {
        Ok(nm) => nm,
        Err(e) => {
            let err_msg = format!("Error during CBOR deserialization: {:?}", e);
            warn!("{}", err_msg);
            NetworkMessage::InvalidMessage
        }
    }
}

#[cfg(test)]
mod unit_test {
    use serde_cbor::ser;
    use std::{
        net::{IpAddr, Ipv4Addr, SocketAddr},
        str::FromStr,
    };

    use super::s11n_network_message;
    use concordium_common::UCursor;

    use crate::{
        common::{P2PNodeId, P2PPeer, P2PPeerBuilder, PeerType},
        network::{
            NetworkId, NetworkMessage, NetworkPacketBuilder, NetworkRequest, NetworkResponse,
        },
    };

    fn localhost_peer() -> P2PPeer {
        P2PPeerBuilder::default()
            .peer_type(PeerType::Node)
            .addr(SocketAddr::new(
                IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)),
                8888,
            ))
            .build()
            .unwrap()
    }

    fn ut_s11n_001_data() -> Vec<(Vec<u8>, NetworkMessage)> {
        let direct_message_content = b"Hello world!";
        let messages = vec![
            NetworkMessage::NetworkRequest(
                NetworkRequest::Ping(localhost_peer()),
                Some(0 as u64),
                None,
            ),
            NetworkMessage::NetworkRequest(
                NetworkRequest::Ping(localhost_peer()),
                Some(11529215046068469760),
                None,
            ),
            NetworkMessage::NetworkResponse(
                NetworkResponse::Pong(localhost_peer()),
                Some(u64::max_value()),
                None,
            ),
            NetworkMessage::NetworkPacket(
                NetworkPacketBuilder::default()
                    .peer(localhost_peer())
                    .message_id(format!("{:064}", 100))
                    .network_id(NetworkId::from(100u16))
                    .message(Box::new(UCursor::from(direct_message_content.to_vec())))
                    .build_direct(P2PNodeId::from_str(&"2A").unwrap())
                    .unwrap(),
                Some(10),
                None,
            ),
        ];

        let mut messages_data: Vec<(Vec<u8>, NetworkMessage)> = vec![];
        for message in messages {
            let data: Vec<u8> = ser::to_vec(&message).unwrap();
            messages_data.push((data, message));
        }

        messages_data
    }

    #[test]
    fn ut_s11n_001() {
        let data = ut_s11n_001_data();
        for (cbor, expected) in &data {
            let output = s11n_network_message(cbor);
            assert_eq!(output, *expected);
        }
    }
}
