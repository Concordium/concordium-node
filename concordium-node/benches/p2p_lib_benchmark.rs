#[macro_use]
extern crate criterion;

use concordium_common::hybrid_buf::HybridBuf;

use p2p_client::{
    common::{P2PPeer, P2PPeerBuilder, PeerType},
    test_utils::{create_random_packet, generate_fake_block, generate_random_data},
};

use std::net::{IpAddr, Ipv4Addr, SocketAddr};

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

#[cfg(any(
    not(feature = "s11n_fbs"),
    not(feature = "s11n_capnp"),
    not(feature = "s11n_serde_cbor"),
))]
mod common {
    use criterion::Criterion;
    pub fn nop_bench(_c: &mut Criterion) {}
}

mod network {
    pub mod deduplication {
        use crate::*;

        use circular_queue::CircularQueue;
        use criterion::Criterion;
        use digest::Digest;
        use twox_hash::XxHash64;

        pub fn bench_dedup_1k(bencher: &mut Criterion) { bench_deduplication(bencher, 250, 1024) }

        pub fn bench_dedup_4k(bencher: &mut Criterion) { bench_deduplication(bencher, 250, 4096) }

        pub fn bench_dedup_16k(bencher: &mut Criterion) {
            bench_deduplication(bencher, 250, 1024 * 16)
        }

        pub fn bench_dedup_32k(bencher: &mut Criterion) {
            bench_deduplication(bencher, 250, 1024 * 32)
        }

        pub fn bench_deduplication(bencher: &mut Criterion, msg_size: usize, queue_size: usize) {
            let bench_id = format!(
                "Deduplication of {}B messages with a {}-elem queue",
                msg_size, queue_size,
            );

            bencher.bench_function(&bench_id, move |b| {
                let mut queue = CircularQueue::with_capacity(queue_size);
                for _ in 0..queue_size {
                    let mut msg_hash = [0u8; 8];
                    msg_hash.copy_from_slice(&XxHash64::digest(&generate_random_data(msg_size)));
                    queue.push(msg_hash);
                }

                b.iter(move || {
                    let new_msg = generate_random_data(msg_size);
                    let mut new_msg_hash = [0u8; 8];
                    new_msg_hash.copy_from_slice(&XxHash64::digest(&new_msg));

                    if !queue.iter().any(|h| h == &new_msg_hash) {
                        queue.push(new_msg_hash);
                    }
                })
            });
        }
    }

    pub mod message {
        use crate::*;
        use concordium_common::serial::Serial;
        use p2p_client::network::{NetworkMessage, NetworkMessagePayload, NetworkResponse};

        use criterion::Criterion;

        pub fn bench_s11n_001_direct_message_256(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 256)
        }

        pub fn bench_s11n_001_direct_message_1k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 1024)
        }

        pub fn bench_s11n_001_direct_message_4k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 4096)
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

        fn bench_s11n_001_direct_message(c: &mut Criterion, size: usize) {
            let msg = create_random_packet(size);
            let mut buffer = HybridBuf::with_capacity(size).unwrap();

            let bench_id = format!("Deserialization of a packet with a {}B payload", size);

            c.bench_function(&bench_id, move |b| {
                b.iter(|| {
                    msg.serial(&mut buffer).unwrap();
                    buffer.rewind().unwrap();
                    NetworkMessage::deserial(&mut buffer).unwrap();
                })
            });
        }
    }
}

mod serialization {
    #[cfg(feature = "s11n_serde_cbor")]
    pub mod serde_cbor {
        use crate::*;
        use p2p_client::network::serialization::cbor::s11n_network_message;

        use criterion::Criterion;
        use serde_cbor::ser;

        fn bench_s11n_001_direct_message(c: &mut Criterion, content_size: usize) {
            let bench_id = format!("Serde CBOR serialization with {}B messages", content_size);

            let msg = create_random_packet(content_size);
            let mut buffer = HybridBuf::with_capacity(content_size).unwrap();

            c.bench_function(&bench_id, move |b| {
                b.iter(|| {
                    ser::to_writer(&mut buffer, &msg).unwrap();
                    buffer.rewind().unwrap();
                    s11n_network_message(&mut buffer)
                })
            });
        }

        pub fn bench_s11n_001_direct_message_256(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 256)
        }

        pub fn bench_s11n_001_direct_message_1k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 1024)
        }

        pub fn bench_s11n_001_direct_message_4k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 4096)
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
    }

    #[cfg(feature = "s11n_capnp")]
    pub mod capnp {
        use crate::*;

        use p2p_client::network::serialization::cap::{deserialize, save_network_message};

        use criterion::Criterion;

        fn bench_s11n_001_direct_message(c: &mut Criterion, content_size: usize) {
            let bench_id = format!("CAPnP serialization with {}B messages", content_size);

            let mut msg = create_random_packet(content_size);
            let mut buffer = HybridBuf::with_capacity(content_size).unwrap();

            c.bench_function(&bench_id, move |b| {
                b.iter(|| {
                    msg.rewind_packet();
                    save_network_message(&mut buffer, &mut msg);
                    buffer.rewind().unwrap();
                    deserialize(&mut buffer)
                })
            });
        }

        pub fn bench_s11n_001_direct_message_256(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 256)
        }

        pub fn bench_s11n_001_direct_message_1k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 1024)
        }

        pub fn bench_s11n_001_direct_message_4k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 4096)
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
    }

    #[cfg(feature = "s11n_fbs")]
    pub mod fbs {
        use crate::*;

        use p2p_client::network::serialization::fbs::{deserialize, serialize};

        use criterion::Criterion;

        fn bench_s11n_001_direct_message(c: &mut Criterion, content_size: usize) {
            let bench_id = format!("Flatbuffers serialization with {}B messages", content_size);

            let mut msg = create_random_packet(content_size);
            let mut buffer = HybridBuf::with_capacity(content_size).unwrap();

            c.bench_function(&bench_id, move |b| {
                b.iter(|| {
                    msg.rewind_packet();
                    serialize(&mut msg, &mut buffer).unwrap();
                    buffer.rewind().unwrap();
                    deserialize(&mut buffer.remaining_bytes()?)
                })
            });
        }

        pub fn bench_s11n_001_direct_message_256(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 256)
        }

        pub fn bench_s11n_001_direct_message_1k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 1024)
        }

        pub fn bench_s11n_001_direct_message_4k(b: &mut Criterion) {
            bench_s11n_001_direct_message(b, 4096)
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
    }
}

criterion_group!(
    s11n_our_benches,
    network::message::bench_s11n_001_direct_message_256,
    network::message::bench_s11n_001_direct_message_1k,
    network::message::bench_s11n_001_direct_message_4k,
    network::message::bench_s11n_001_direct_message_64k,
    network::message::bench_s11n_001_direct_message_256k,
    network::message::bench_s11n_001_direct_message_1m,
    network::message::bench_s11n_001_direct_message_4m,
);

#[cfg(feature = "s11n_capnp")]
criterion_group!(
    s11n_capnp_benches,
    serialization::capnp::bench_s11n_001_direct_message_256,
    serialization::capnp::bench_s11n_001_direct_message_1k,
    serialization::capnp::bench_s11n_001_direct_message_4k,
    serialization::capnp::bench_s11n_001_direct_message_64k,
    serialization::capnp::bench_s11n_001_direct_message_256k,
    serialization::capnp::bench_s11n_001_direct_message_1m,
    serialization::capnp::bench_s11n_001_direct_message_4m,
);
#[cfg(not(feature = "s11n_capnp"))]
criterion_group!(s11n_capnp_benches, common::nop_bench);

#[cfg(feature = "s11n_fbs")]
criterion_group!(
    s11n_fbs_benches,
    serialization::fbs::bench_s11n_001_direct_message_256,
    serialization::fbs::bench_s11n_001_direct_message_1k,
    serialization::fbs::bench_s11n_001_direct_message_4k,
    serialization::fbs::bench_s11n_001_direct_message_64k,
    serialization::fbs::bench_s11n_001_direct_message_256k,
    serialization::fbs::bench_s11n_001_direct_message_1m,
    serialization::fbs::bench_s11n_001_direct_message_4m,
);
#[cfg(not(feature = "s11n_fbs"))]
criterion_group!(s11n_fbs_benches, common::nop_bench);

#[cfg(feature = "s11n_serde_cbor")]
criterion_group!(
    s11n_cbor_benches,
    serialization::serde_cbor::bench_s11n_001_direct_message_256,
    serialization::serde_cbor::bench_s11n_001_direct_message_1k,
    serialization::serde_cbor::bench_s11n_001_direct_message_4k,
    serialization::serde_cbor::bench_s11n_001_direct_message_64k,
    serialization::serde_cbor::bench_s11n_001_direct_message_256k,
    serialization::serde_cbor::bench_s11n_001_direct_message_1m,
    serialization::serde_cbor::bench_s11n_001_direct_message_4m,
);
#[cfg(not(feature = "s11n_serde_cbor"))]
criterion_group!(s11n_cbor_benches, common::nop_bench);

criterion_group!(
    name = dedup;
    config = network::connection::bench_config(10);
    targets = network::deduplication::bench_dedup_1k, network::deduplication::bench_dedup_4k,
    network::deduplication::bench_dedup_16k, network::deduplication::bench_dedup_32k
);

criterion_main!(
    // dedup,
    s11n_fbs_benches,
    s11n_capnp_benches,
    s11n_cbor_benches,
    s11n_our_benches,
);
