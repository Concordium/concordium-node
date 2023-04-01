use semver::Version;

use crate::{
    common::{get_current_stamp, p2p_peer::P2PPeer, P2PNodeId, PeerType},
    network::{
        Handshake, NetworkId, NetworkMessage, NetworkPayload, NetworkRequest, NetworkResponse,
    },
    test_utils::{create_random_packet, dummy_regenesis_blocks},
};

use std::{
    io::Cursor,
    net::{IpAddr, SocketAddr},
};

macro_rules! test_s11n {
    ($name:ident, $payload:expr) => {
        #[test]
        fn $name() {
            let msg = NetworkMessage {
                created:  get_current_stamp(),
                received: None,
                payload:  $payload,
            };
            let mut buffer = Cursor::new(Vec::new());

            msg.serialize(&mut buffer).unwrap();
            let deserialized = NetworkMessage::deserialize(&buffer.get_ref()).unwrap();
            assert_eq!(deserialized.payload, msg.payload);
        }
    };
}

test_s11n!(s11n_req_ping, NetworkPayload::NetworkRequest(NetworkRequest::Ping));
test_s11n!(
    s11n_req_get_peers,
    NetworkPayload::NetworkRequest(NetworkRequest::GetPeers(
        [100u16, 1000, 1234, 9999].iter().copied().map(NetworkId::from).collect(),
    ))
);
test_s11n!(
    s11n_req_handshake,
    NetworkPayload::NetworkRequest(NetworkRequest::Handshake(Handshake {
        remote_id:      P2PNodeId(77),
        remote_port:    1234,
        networks:       [100u16, 1000, 1234, 9999].iter().copied().map(NetworkId::from).collect(),
        node_version:   Version::parse(env!("CARGO_PKG_VERSION")).unwrap(),
        wire_versions:  vec![0, 1, 2],
        genesis_blocks: dummy_regenesis_blocks(),
        proof:          Vec::new(),
    }))
);
test_s11n!(
    s11n_req_join_net,
    NetworkPayload::NetworkRequest(NetworkRequest::JoinNetwork(NetworkId::from(1337),))
);
test_s11n!(
    s11n_req_leave_net,
    NetworkPayload::NetworkRequest(NetworkRequest::LeaveNetwork(NetworkId::from(1337),))
);

test_s11n!(s11n_resp_pong, NetworkPayload::NetworkResponse(NetworkResponse::Pong));

test_s11n!(
    s11n_resp_peer_list,
    NetworkPayload::NetworkResponse(NetworkResponse::PeerList(
        [
            P2PPeer {
                id:        P2PNodeId(1234567890123),
                addr:      SocketAddr::new(IpAddr::from([1, 2, 3, 4]), 80),
                peer_type: PeerType::Bootstrapper,
            },
            P2PPeer {
                id:        P2PNodeId(1),
                addr:      SocketAddr::new(IpAddr::from([8, 7, 6, 5, 4, 3, 2, 1]), 8080),
                peer_type: PeerType::Node,
            },
        ]
        .to_vec(),
    ))
);

#[test]
fn s11n_packet() {
    let msg = create_random_packet(8);
    let mut buffer = Cursor::new(Vec::new());

    msg.serialize(&mut buffer).unwrap();
    let deserialized = NetworkMessage::deserialize(buffer.get_ref()).unwrap();
    assert_eq!(deserialized.payload, msg.payload);
}

quickcheck! {
    fn s11n_fuzzed(bytes: Vec<u8>) -> bool {
        let _ = NetworkMessage::deserialize(&bytes);
        true
    }
}
