use crate::network::NetworkMessage;

pub fn s11n_network_message(input: &str) -> NetworkMessage {
    match serde_json::from_str::<NetworkMessage>(input) {
        Ok(nm) => nm,
        Err(e) => {
            let err_msg = format!("Error during JSON deserialization: {:?}", e);
            warn!("{}", err_msg);
            NetworkMessage::InvalidMessage
        }
    }
}

#[cfg(test)]
mod unit_test {
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

    fn ut_s11n_001_data() -> Vec<(String, NetworkMessage)> {
        let direct_message_content = UCursor::from(b"Hello world!".to_vec());
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
                    .message(Box::new(direct_message_content))
                    .build_direct(P2PNodeId::from_str(&"2A").unwrap())
                    .unwrap(),
                Some(10),
                None,
            ),
        ];

        let mut messages_data: Vec<(String, NetworkMessage)> = Vec::with_capacity(messages.len());
        for message in messages {
            let json: String = serde_json::to_string(&message).unwrap();
            messages_data.push((json, message));
        }

        messages_data
    }

    #[test]
    fn ut_s11n_001() {
        let data = ut_s11n_001_data();
        for (json, expected) in &data {
            let output = s11n_network_message(json);
            assert_eq!(output, *expected)
        }
    }
}
