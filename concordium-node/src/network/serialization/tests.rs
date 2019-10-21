use crate::{
    common::{p2p_peer::P2PPeer, P2PNodeId, PeerType},
    network::{NetworkId, NetworkMessage, NetworkMessagePayload, NetworkRequest, NetworkResponse},
    p2p::banned_nodes::BannedNode,
    test_utils::create_random_packet,
};

use std::{
    io::Cursor,
    net::{IpAddr, SocketAddr},
};

macro_rules! test_s11n {
    ($name:ident, $payload:expr) => {
        #[test]
        fn $name() {
            let mut msg = NetworkMessage {
                timestamp1: None,
                timestamp2: None,
                payload:    $payload,
            };
            let mut buffer = Cursor::new(Vec::new());

            msg.serialize(&mut buffer).unwrap();
            let deserialized = NetworkMessage::deserialize(&buffer.get_ref()).unwrap();
            assert_eq!(deserialized.payload, msg.payload);
        }
    };
}

test_s11n!(
    s11n_req_ping,
    NetworkMessagePayload::NetworkRequest(NetworkRequest::Ping)
);
test_s11n!(
    s11n_req_get_peers,
    NetworkMessagePayload::NetworkRequest(NetworkRequest::GetPeers(
        [100u16, 1000, 1234, 9999]
            .iter()
            .copied()
            .map(NetworkId::from)
            .collect(),
    ))
);
test_s11n!(
    s11n_req_handshake,
    NetworkMessagePayload::NetworkRequest(NetworkRequest::Handshake(
        P2PNodeId(77),
        1234,
        [100u16, 1000, 1234, 9999]
            .iter()
            .copied()
            .map(NetworkId::from)
            .collect(),
        Vec::new(),
    ))
);
test_s11n!(
    s11n_req_ban_id,
    NetworkMessagePayload::NetworkRequest(NetworkRequest::BanNode(BannedNode::ById(P2PNodeId(
        1337
    )),))
);
test_s11n!(
    s11n_req_unban_id,
    NetworkMessagePayload::NetworkRequest(NetworkRequest::UnbanNode(BannedNode::ById(P2PNodeId(
        1337
    )),))
);
test_s11n!(
    s11n_req_ban_ip_v4,
    NetworkMessagePayload::NetworkRequest(NetworkRequest::BanNode(BannedNode::ByAddr(
        IpAddr::from([4, 3, 2, 1])
    ),))
);
test_s11n!(
    s11n_req_ban_ip_v6,
    NetworkMessagePayload::NetworkRequest(NetworkRequest::BanNode(BannedNode::ByAddr(
        IpAddr::from([1, 2, 3, 4, 5, 6, 7, 8])
    ),))
);
test_s11n!(
    s11n_req_join_net,
    NetworkMessagePayload::NetworkRequest(NetworkRequest::JoinNetwork(NetworkId::from(1337),))
);
test_s11n!(
    s11n_req_leave_net,
    NetworkMessagePayload::NetworkRequest(NetworkRequest::LeaveNetwork(NetworkId::from(1337),))
);

// TODO: Retransmit (Requests)

test_s11n!(
    s11n_resp_pong,
    NetworkMessagePayload::NetworkResponse(NetworkResponse::Pong)
);

test_s11n!(
    s11n_resp_peer_list,
    NetworkMessagePayload::NetworkResponse(NetworkResponse::PeerList(
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
        .iter()
        .cloned()
        .collect(),
    ))
);

test_s11n!(
    s11n_resp_handshake,
    NetworkMessagePayload::NetworkResponse(NetworkResponse::Handshake(
        P2PNodeId(77),
        1234,
        [100u16, 1000, 1234, 9999]
            .iter()
            .copied()
            .map(NetworkId::from)
            .collect(),
        Vec::new(),
    ))
);

#[test]
fn s11n_packet() {
    let mut msg = create_random_packet(8);
    let mut buffer = Cursor::new(Vec::new());

    msg.serialize(&mut buffer).unwrap();
    let deserialized = NetworkMessage::deserialize(&buffer.get_ref()).unwrap();
    assert_eq!(deserialized.payload, msg.payload);
}

quickcheck! {
    fn s11n_fuzzed(bytes: Vec<u8>) -> bool {
        let _ = NetworkMessage::deserialize(&bytes);
        true
    }
}
