#[macro_use]
extern crate criterion;

use concordium_common::UCursor;

use p2p_client::{
    common::{
        get_current_stamp,
        serialization::{Serializable, WriteArchiveAdapter},
        P2PNodeId, P2PPeer, P2PPeerBuilder, PeerType,
    },
    network::{NetworkId, NetworkMessage, NetworkPacket, NetworkPacketBuilder},
};

use failure::Fallible;
use rand::{distributions::Alphanumeric, thread_rng, Rng};

use std::{
    io::{Seek, SeekFrom, Write},
    net::{IpAddr, Ipv4Addr, SocketAddr},
    str::FromStr,
};

pub fn localhost_peer() -> P2PPeer {
    P2PPeerBuilder::default()
        .peer_type(PeerType::Node)
        .addr(SocketAddr::new(
            IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)),
            8888,
        ))
        .build()
        .unwrap()
}

pub fn make_direct_message_into_disk(content_size: usize) -> Fallible<UCursor> {
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

    // 2. Generate packet.
    let p2p_node_id = P2PNodeId::from_str("000000002dd2b6ed")?;
    let pkt = NetworkPacketBuilder::default()
        .peer(P2PPeer::from(
            PeerType::Node,
            p2p_node_id,
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

#[cfg(any(
    not(feature = "s11n_nom"),
    not(feature = "s11n_capnp"),
    not(feature = "s11n_serde_cbor"),
    not(feature = "s11n_serde_json")
))]
mod common {
    use criterion::Criterion;
    pub fn nop_bench(_c: &mut Criterion) {}
}

mod network {
    pub mod message {
        use crate::{localhost_peer, make_direct_message_into_disk};
        use concordium_common::{ContainerView, UCursor};
        use p2p_client::{
            common::{
                get_current_stamp,
                serialization::{
                    Deserializable, ReadArchiveAdapter, Serializable, WriteArchiveAdapter,
                },
                P2PPeerBuilder, PeerType, RemotePeer,
            },
            network::{NetworkMessage, NetworkResponse},
        };
        use std::net::{IpAddr, Ipv4Addr, SocketAddr};

        use criterion::Criterion;

        pub fn bench_s11n_001_direct_message_256(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 256)
        }

        pub fn bench_s11n_001_direct_message_512(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 512)
        }

        pub fn bench_s11n_001_direct_message_1k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 1024)
        }

        pub fn bench_s11n_001_direct_message_4k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 4096)
        }

        pub fn bench_s11n_001_direct_message_32k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 32 * 1024)
        }

        pub fn bench_s11n_001_direct_message_64k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 64 * 1024)
        }

        pub fn bench_s11n_001_direct_message_256k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 256 * 1024)
        }

        pub fn bench_s11n_001_direct_message_1m(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 1024 * 1024)
        }

        pub fn bench_s11n_001_direct_message_4m(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 4 * 1024 * 1024)
        }

        fn bench_s11n_001_direct_message(c: &mut Criterion, content_size: usize) {
            let mut cursor = make_direct_message_into_disk(content_size).unwrap();
            cursor
                .swap_to_memory()
                .expect("Cannot move cursor to memory");

            let local_ip = IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1));
            let local_peer = P2PPeerBuilder::default()
                .peer_type(PeerType::Node)
                .addr(SocketAddr::new(local_ip, 8888))
                .build()
                .unwrap();
            let bench_id = format!(
                "Deserialization of DirectMessages with a {}B payload",
                content_size
            );

            c.bench_function(&bench_id, move |b| {
                let cloned_cursor = cursor.clone();
                let peer = RemotePeer::PostHandshake(local_peer.clone());
                let ip = local_ip;

                b.iter(move || {
                    let mut archive =
                        ReadArchiveAdapter::new(cloned_cursor.clone(), peer.clone(), ip);
                    NetworkMessage::deserialize(&mut archive)
                })
            });
        }

        pub fn bench_s11n_001_direct_message_8m(b: &mut Criterion) {
            bench_s11n_001_direct_message_from_disk(b, 8 * 1024 * 1024)
        }

        pub fn bench_s11n_001_direct_message_128m(b: &mut Criterion) {
            bench_s11n_001_direct_message_from_disk(b, 128 * 1024 * 1024)
        }

        pub fn bench_s11n_001_direct_message_256m(b: &mut Criterion) {
            bench_s11n_001_direct_message_from_disk(b, 256 * 1024 * 1024)
        }

        pub fn bench_s11n_001_direct_message_512m(b: &mut Criterion) {
            bench_s11n_001_direct_message_from_disk(b, 512 * 1024 * 1024)
        }

        pub fn bench_s11n_001_direct_message_1g(b: &mut Criterion) {
            bench_s11n_001_direct_message_from_disk(b, 1024 * 1024 * 1024)
        }

        pub fn bench_s11n_001_direct_message_4g(b: &mut Criterion) {
            bench_s11n_001_direct_message_from_disk(b, 4 * 1024 * 1024 * 1024)
        }

        fn bench_s11n_001_direct_message_from_disk(c: &mut Criterion, content_size: usize) {
            // Create serialization data in memory and then move to disk
            let cursor_on_disk = make_direct_message_into_disk(content_size).unwrap();

            // Local stuff
            let local_ip = IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1));
            let local_peer = P2PPeerBuilder::default()
                .peer_type(PeerType::Node)
                .addr(SocketAddr::new(local_ip.clone(), 8888))
                .build()
                .unwrap();
            let bench_id = format!(
                "Deserialization of DirectMessages with a {}B payload using temporary files",
                content_size
            );

            c.bench_function(&bench_id, move |b| {
                let cursor = cursor_on_disk.clone();
                let peer = RemotePeer::PostHandshake(local_peer.clone());

                b.iter(move || {
                    let mut archive =
                        ReadArchiveAdapter::new(cursor.clone(), peer.clone(), local_ip);

                    NetworkMessage::deserialize(&mut archive)
                })
            });
        }

        pub fn bench_s11n_get_peers_50(c: &mut Criterion) { bench_s11n_get_peers(c, 50) }

        pub fn bench_s11n_get_peers_100(c: &mut Criterion) { bench_s11n_get_peers(c, 100) }

        pub fn bench_s11n_get_peers_200(c: &mut Criterion) { bench_s11n_get_peers(c, 200) }

        fn bench_s11n_get_peers(c: &mut Criterion, size: usize) {
            let me = localhost_peer();
            let mut peers = vec![];
            peers.resize_with(size, || localhost_peer());

            let local_ip = IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1));
            let peer_list_msg = NetworkMessage::NetworkResponse(
                NetworkResponse::PeerList(me.clone(), peers),
                Some(get_current_stamp()),
                None,
            );

            let bench_id = format!("Deserialization of PeerList responses with {} peers ", size);

            c.bench_function(&bench_id, move |b| {
                let mut archive = WriteArchiveAdapter::from(vec![]);
                let _ = peer_list_msg.serialize(&mut archive).unwrap();
                let peer_list_msg_data = ContainerView::from(archive.into_inner());

                let cursor = UCursor::build_from_view(peer_list_msg_data);
                let peer = me.clone();

                b.iter(move || {
                    let remote_peer = RemotePeer::PostHandshake(peer.clone());
                    let mut archive =
                        ReadArchiveAdapter::new(cursor.clone(), remote_peer, local_ip);
                    NetworkMessage::deserialize(&mut archive).unwrap()
                })
            });
        }
    }

    pub mod connection {
        use concordium_common::UCursor;
        use criterion::Criterion;
        use p2p_client::{
            common::PeerType,
            network::NetworkId,
            p2p::p2p_node::send_message_from_cursor,
            test_utils::{
                await_handshake, connect, make_node_and_sync, next_available_port, setup_logger,
                wait_direct_message,
            },
        };
        use rand::{distributions::Standard, thread_rng, Rng};

        pub fn bench_config(sample_size: usize) -> Criterion {
            Criterion::default().sample_size(sample_size)
        }

        // P2P Communication Benchmark
        // ============================
        pub fn p2p_net_64b(c: &mut Criterion) { p2p_net(c, 64); }
        pub fn p2p_net_4k(c: &mut Criterion) { p2p_net(c, 4 * 1024); }
        pub fn p2p_net_64k(c: &mut Criterion) { p2p_net(c, 64 * 1024); }
        pub fn p2p_net_8m(c: &mut Criterion) { p2p_net(c, 8 * 1024 * 1024); }
        pub fn p2p_net_32m(c: &mut Criterion) { p2p_net(c, 32 * 1024 * 1024); }
        pub fn p2p_net_128m(c: &mut Criterion) { p2p_net(c, 128 * 1024 * 1024); }

        fn p2p_net(c: &mut Criterion, size: usize) {
            setup_logger();

            // Create nodes and connect them.
            let (mut node_1, msg_waiter_1) =
                make_node_and_sync(next_available_port(), vec![100], PeerType::Node).unwrap();
            let (node_2, msg_waiter_2) =
                make_node_and_sync(next_available_port(), vec![100], PeerType::Node).unwrap();

            connect(&mut node_1, &node_2).unwrap();
            await_handshake(&msg_waiter_1).unwrap();

            // let mut msg = make_direct_message_into_disk().unwrap();
            let msg = thread_rng()
                .sample_iter(&Standard)
                .take(size)
                .collect::<Vec<u8>>();
            let uc = UCursor::from(msg);
            let bench_id = format!("P2P network using {}B messages", size);

            c.bench_function(&bench_id, move |b| {
                let net_id = NetworkId::from(100);

                b.iter(|| {
                    // Send.
                    send_message_from_cursor(
                        &node_1,
                        Some(node_2.id()),
                        vec![],
                        net_id,
                        None,
                        uc.clone(),
                        false,
                    )
                    .unwrap();
                    let msg_recv = wait_direct_message(&msg_waiter_2).unwrap();
                    assert_eq!(uc.len(), msg_recv.len());
                });
            });
        }
    }
}

mod serialization {
    #[cfg(feature = "s11n_serde_cbor")]
    pub mod serde_cbor {
        use crate::localhost_peer;
        use concordium_common::UCursor;
        use p2p_client::{
            common::P2PNodeId,
            network::{
                packet::MessageId, serialization::cbor::s11n_network_message, NetworkId,
                NetworkMessage, NetworkPacketBuilder,
            },
        };

        use rand::{distributions::Alphanumeric, thread_rng, Rng};
        use std::str::FromStr;

        use criterion::Criterion;
        use serde_cbor::ser;

        fn bench_s11n_001_direct_message(c: &mut Criterion, content_size: usize) {
            let content: String = thread_rng()
                .sample_iter(&Alphanumeric)
                .take(content_size)
                .collect();

            let dm = NetworkMessage::NetworkPacket(
                NetworkPacketBuilder::default()
                    .peer(localhost_peer())
                    .message_id(MessageId::new(&[0u8; 32]))
                    .network_id(NetworkId::from(100u16))
                    .message(UCursor::from(content.into_bytes()))
                    .build_direct(P2PNodeId::from_str(&"2A").unwrap())
                    .unwrap(),
                Some(10),
                None,
            );

            let data: Vec<u8> = ser::to_vec(&dm).unwrap();

            let bench_id = format!("Serde CBOR serialization with {}B messages", content_size);
            c.bench_function(&bench_id, move |b| {
                let local_data = data.as_slice();
                b.iter(move || s11n_network_message(local_data))
            });
        }

        pub fn bench_s11n_001_direct_message_256(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 256)
        }

        pub fn bench_s11n_001_direct_message_512(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 512)
        }

        pub fn bench_s11n_001_direct_message_1k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 1024)
        }

        pub fn bench_s11n_001_direct_message_4k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 4096)
        }

        pub fn bench_s11n_001_direct_message_32k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 32 * 1024)
        }

        pub fn bench_s11n_001_direct_message_64k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 64 * 1024)
        }

        pub fn bench_s11n_001_direct_message_256k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 256 * 1024)
        }
    }

    #[cfg(feature = "s11n_serde_json")]
    pub mod serde_json {
        use crate::localhost_peer;
        use concordium_common::UCursor;

        use p2p_client::{
            common::P2PNodeId,
            network::{
                packet::MessageId, serialization::json::s11n_network_message, NetworkId,
                NetworkMessage, NetworkPacketBuilder,
            },
        };

        use rand::{distributions::Standard, thread_rng, Rng};
        use std::str::FromStr;

        use criterion::Criterion;

        fn bench_s11n_001_direct_message(c: &mut Criterion, content_size: usize) {
            let content: Vec<u8> = thread_rng()
                .sample_iter(&Standard)
                .take(content_size)
                .collect();
            let content_cursor = UCursor::from(content);

            let dm = NetworkMessage::NetworkPacket(
                NetworkPacketBuilder::default()
                    .peer(localhost_peer())
                    .message_id(MessageId::new(&[0u8; 32]))
                    .network_id(NetworkId::from(100u16))
                    .message(content_cursor)
                    .build_direct(P2PNodeId::from_str(&"2A").unwrap())
                    .unwrap(),
                Some(10),
                None,
            );

            let data: String = serde_json::to_string(&dm).unwrap();

            let bench_id = format!("Serde serialization JSON with {}B messages", content_size);
            c.bench_function(&bench_id, move |b| {
                let data_raw: &str = data.as_str();
                b.iter(move || s11n_network_message(data_raw))
            });
        }

        pub fn bench_s11n_001_direct_message_256(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 256)
        }

        pub fn bench_s11n_001_direct_message_512(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 512)
        }

        pub fn bench_s11n_001_direct_message_1k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 1024)
        }

        pub fn bench_s11n_001_direct_message_4k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 4096)
        }

        pub fn bench_s11n_001_direct_message_32k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 32 * 1024)
        }

        pub fn bench_s11n_001_direct_message_64k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 64 * 1024)
        }

        pub fn bench_s11n_001_direct_message_256k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 256 * 1024)
        }
    }

    #[cfg(feature = "s11n_nom")]
    pub mod nom {
        use crate::localhost_peer;
        use p2p_client::network::{
            serialization::nom::s11n_network_message, NetworkId, NetworkPacket,
            ProtocolMessageType, ProtocolPacketType, PROTOCOL_NAME,
        };

        use rand::{distributions::Alphanumeric, thread_rng, Rng};

        use criterion::Criterion;

        fn bench_s11n_001_direct_message(c: &mut Criterion, content_size: usize) {
            let header = format!(
                "{}{}{}{}{}{}{}{}{:010}",
                PROTOCOL_NAME,
                "001",
                base64::encode(&10u64.to_le_bytes()[..]),
                ProtocolMessageType::Packet,
                ProtocolPacketType::Direct,
                localhost_peer().id(),
                NetworkPacket::generate_message_id(),
                NetworkId::from(111u16),
                content_size
            );

            let content: String = thread_rng()
                .sample_iter(&Alphanumeric)
                .take(content_size)
                .collect();

            let pkt: String = [header, content].concat();

            let bench_id = format!("NOM serialization with {}B messages", content_size);
            c.bench_function(&bench_id, move |b| {
                let data = pkt.as_bytes();
                b.iter(move || s11n_network_message(data));
            });
        }

        pub fn bench_s11n_001_direct_message_256(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 256)
        }

        pub fn bench_s11n_001_direct_message_512(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 512)
        }

        pub fn bench_s11n_001_direct_message_1k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 1024)
        }

        pub fn bench_s11n_001_direct_message_4k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 4096)
        }

        pub fn bench_s11n_001_direct_message_32k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 32 * 1024)
        }

        pub fn bench_s11n_001_direct_message_64k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 64 * 1024)
        }

        pub fn bench_s11n_001_direct_message_256k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 256 * 1024)
        }
    }

    #[cfg(feature = "s11n_capnp")]
    pub mod capnp {
        use crate::localhost_peer;

        use concordium_common::UCursor;

        use p2p_client::{
            common::{P2PNodeId, P2PPeerBuilder, PeerType},
            network::{
                packet::MessageId,
                serialization::cap::{deserialize, save_network_message},
                NetworkId, NetworkMessage, NetworkPacketBuilder,
            },
        };

        use criterion::Criterion;
        use std::str::FromStr;

        use rand::{distributions::Standard, thread_rng, Rng};
        use std::net::{IpAddr, Ipv4Addr, SocketAddr};

        fn bench_s11n_001_direct_message(c: &mut Criterion, content_size: usize) {
            let content: Vec<u8> = thread_rng()
                .sample_iter(&Standard)
                .take(content_size)
                .collect();
            let content_cursor = UCursor::from(content);

            let local_ip = IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1));
            let local_peer = P2PPeerBuilder::default()
                .peer_type(PeerType::Node)
                .addr(SocketAddr::new(local_ip, 8888))
                .build()
                .unwrap();

            let mut dm = NetworkMessage::NetworkPacket(
                NetworkPacketBuilder::default()
                    .peer(localhost_peer())
                    .message_id(MessageId::new(&[0u8; 32]))
                    .network_id(NetworkId::from(111u16))
                    .message(content_cursor)
                    .build_direct(P2PNodeId::from_str(&"2A").unwrap())
                    .unwrap(),
                Some(10),
                None,
            );

            let data: Vec<u8> = save_network_message(&mut dm);

            let bench_id = format!("CAPnP serialization with {}B messages", content_size);
            c.bench_function(&bench_id, move |b| {
                let data_raw: &[u8] = data.as_slice();
                b.iter(|| deserialize(&local_peer, &local_ip, data_raw))
            });
        }

        pub fn bench_s11n_001_direct_message_256(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 256)
        }

        pub fn bench_s11n_001_direct_message_512(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 512)
        }

        pub fn bench_s11n_001_direct_message_1k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 1024)
        }

        pub fn bench_s11n_001_direct_message_4k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 4096)
        }

        pub fn bench_s11n_001_direct_message_32k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 32 * 1024)
        }

        pub fn bench_s11n_001_direct_message_64k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 64 * 1024)
        }

        pub fn bench_s11n_001_direct_message_256k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 256 * 1024)
        }

    }
}

criterion_group!(
    s11n_custom_benches,
    network::message::bench_s11n_001_direct_message_256,
    network::message::bench_s11n_001_direct_message_512,
    network::message::bench_s11n_001_direct_message_1k,
    network::message::bench_s11n_001_direct_message_4k,
    network::message::bench_s11n_001_direct_message_32k,
    network::message::bench_s11n_001_direct_message_64k,
    network::message::bench_s11n_001_direct_message_256k,
    network::message::bench_s11n_001_direct_message_1m,
    network::message::bench_s11n_001_direct_message_4m,
    network::message::bench_s11n_001_direct_message_8m,
    network::message::bench_s11n_001_direct_message_128m,
    network::message::bench_s11n_001_direct_message_256m,
    network::message::bench_s11n_001_direct_message_512m,
    network::message::bench_s11n_001_direct_message_1g,
    network::message::bench_s11n_001_direct_message_4g,
);

criterion_group!(
    s11n_get_peers,
    network::message::bench_s11n_get_peers_50,
    network::message::bench_s11n_get_peers_100,
    network::message::bench_s11n_get_peers_200
);

#[cfg(feature = "s11n_serde_cbor")]
criterion_group!(
    s11n_cbor_benches,
    serialization::serde_cbor::bench_s11n_001_direct_message_256,
    serialization::serde_cbor::bench_s11n_001_direct_message_512,
    serialization::serde_cbor::bench_s11n_001_direct_message_1k,
    serialization::serde_cbor::bench_s11n_001_direct_message_4k,
    serialization::serde_cbor::bench_s11n_001_direct_message_32k,
    serialization::serde_cbor::bench_s11n_001_direct_message_64k,
    serialization::serde_cbor::bench_s11n_001_direct_message_256k,
);
#[cfg(not(feature = "s11n_serde_cbor"))]
criterion_group!(s11n_cbor_benches, common::nop_bench);

#[cfg(feature = "s11n_serde_json")]
criterion_group!(
    s11n_json_benches,
    serialization::serde_json::bench_s11n_001_direct_message_256,
    serialization::serde_json::bench_s11n_001_direct_message_512,
    serialization::serde_json::bench_s11n_001_direct_message_1k,
    serialization::serde_json::bench_s11n_001_direct_message_4k,
    serialization::serde_json::bench_s11n_001_direct_message_32k,
    serialization::serde_json::bench_s11n_001_direct_message_64k,
    serialization::serde_json::bench_s11n_001_direct_message_256k,
);
#[cfg(not(feature = "s11n_serde_json"))]
criterion_group!(s11n_json_benches, common::nop_bench);

#[cfg(feature = "s11n_nom")]
criterion_group!(
    s11n_nom_benches,
    serialization::nom::bench_s11n_001_direct_message_256,
    serialization::nom::bench_s11n_001_direct_message_512,
    serialization::nom::bench_s11n_001_direct_message_1k,
    serialization::nom::bench_s11n_001_direct_message_4k,
    serialization::nom::bench_s11n_001_direct_message_32k,
    serialization::nom::bench_s11n_001_direct_message_64k,
    serialization::nom::bench_s11n_001_direct_message_256k,
);
#[cfg(not(feature = "s11n_nom"))]
criterion_group!(s11n_nom_benches, common::nop_bench);

#[cfg(feature = "s11n_capnp")]
criterion_group!(
    s11n_capnp_benches,
    serialization::capnp::bench_s11n_001_direct_message_256,
    serialization::capnp::bench_s11n_001_direct_message_512,
    serialization::capnp::bench_s11n_001_direct_message_1k,
    serialization::capnp::bench_s11n_001_direct_message_4k,
    serialization::capnp::bench_s11n_001_direct_message_32k,
    serialization::capnp::bench_s11n_001_direct_message_64k,
    serialization::capnp::bench_s11n_001_direct_message_256k,
);
#[cfg(not(feature = "s11n_capnp"))]
criterion_group!(s11n_capnp_benches, common::nop_bench);

criterion_group!(
    name = p2p_net_small_benches;
    config = network::connection::bench_config(10);
    targets = network::connection::p2p_net_64b, network::connection::p2p_net_4k,
    network::connection::p2p_net_64k );

criterion_group!(
    name = p2p_net_big_benches;
    config = network::connection::bench_config(10);
    targets = network::connection::p2p_net_8m,
    network::connection::p2p_net_32m,
    network::connection::p2p_net_128m
);

criterion_main!(
    p2p_net_small_benches,
    p2p_net_big_benches,
    s11n_get_peers,
    s11n_custom_benches,
    s11n_cbor_benches,
    s11n_json_benches,
    s11n_nom_benches,
    s11n_capnp_benches
);
