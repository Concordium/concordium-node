#[macro_use]
extern crate criterion;

use criterion::Criterion;

use p2p_client::{
    common::{P2PPeer, P2PPeerBuilder, PeerType},
    test_utils::{create_random_packet, generate_random_data},
};

use std::{
    io::{Cursor, Seek, SeekFrom},
    net::{IpAddr, Ipv4Addr, SocketAddr},
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

#[cfg(any(not(feature = "s11n_capnp"), not(feature = "s11n_serde")))]
mod common {
    use criterion::Criterion;
    pub fn nop_bench(_c: &mut Criterion) {}
}

pub mod deduplication {
    use crate::*;
    use circular_queue::CircularQueue;
    use digest::Digest;
    use twox_hash::XxHash64;

    pub fn bench_dedup(bencher: &mut Criterion, msg_size: usize, queue_size: usize) {
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

    pub fn bench_dedup_1k(bencher: &mut Criterion) { bench_dedup(bencher, 250, 1024) }
    pub fn bench_dedup_4k(bencher: &mut Criterion) { bench_dedup(bencher, 250, 4096) }
    pub fn bench_dedup_16k(bencher: &mut Criterion) { bench_dedup(bencher, 250, 1024 * 16) }
    pub fn bench_dedup_32k(bencher: &mut Criterion) { bench_dedup(bencher, 250, 1024 * 32) }
}

mod s11n {
    #[cfg(not(feature = "s11n_serde"))]
    pub mod fbs {
        use crate::*;
        use p2p_client::network::NetworkMessage;

        fn bench_s11n(c: &mut Criterion, size: usize) {
            let bench_id = format!("Flatbuffers serialization with {}B messages", size);

            let mut msg = create_random_packet(size);
            let mut buffer = Cursor::new(Vec::with_capacity(size));

            c.bench_function(&bench_id, move |b| {
                b.iter(|| {
                    msg.rewind_packet();
                    msg.serialize(&mut buffer).unwrap();
                    NetworkMessage::deserialize(&buffer.get_ref()).unwrap();
                    buffer.seek(SeekFrom::Start(0)).unwrap();
                })
            });
        }

        pub fn bench_s11n_256(b: &mut Criterion) { bench_s11n(b, 256) }
        pub fn bench_s11n_1k(b: &mut Criterion) { bench_s11n(b, 1024) }
        pub fn bench_s11n_4k(b: &mut Criterion) { bench_s11n(b, 4096) }
        pub fn bench_s11n_64k(b: &mut Criterion) { bench_s11n(b, 64 * 1024) }
        pub fn bench_s11n_256k(b: &mut Criterion) { bench_s11n(b, 256 * 1024) }
        pub fn bench_s11n_1m(b: &mut Criterion) { bench_s11n(b, 1024 * 1024) }
        pub fn bench_s11n_4m(b: &mut Criterion) { bench_s11n(b, 4 * 1024 * 1024) }
    }

    #[cfg(feature = "s11n_serde_cbor")]
    pub mod serde_cbor {
        use crate::*;
        use p2p_client::network::NetworkMessage;

        fn bench_s11n(c: &mut Criterion, size: usize) {
            let bench_id = format!("Serde CBOR serialization with {}B messages", size);

            let mut msg = create_random_packet(size);
            let mut buffer = Cursor::new(Vec::with_capacity(size));

            c.bench_function(&bench_id, move |b| {
                b.iter(|| {
                    msg.rewind_packet();
                    msg.serialize(&mut buffer).unwrap();
                    NetworkMessage::deserialize(&buffer.get_ref()).unwrap();
                    buffer.seek(SeekFrom::Start(0)).unwrap();
                })
            });
        }

        pub fn bench_s11n_256(b: &mut Criterion) { bench_s11n(b, 256) }
        pub fn bench_s11n_1k(b: &mut Criterion) { bench_s11n(b, 1024) }
        pub fn bench_s11n_4k(b: &mut Criterion) { bench_s11n(b, 4096) }
        pub fn bench_s11n_64k(b: &mut Criterion) { bench_s11n(b, 64 * 1024) }
        pub fn bench_s11n_256k(b: &mut Criterion) { bench_s11n(b, 256 * 1024) }
        pub fn bench_s11n_1m(b: &mut Criterion) { bench_s11n(b, 1024 * 1024) }
        pub fn bench_s11n_4m(b: &mut Criterion) { bench_s11n(b, 4 * 1024 * 1024) }
    }

    #[cfg(feature = "s11n_serde_msgpack")]
    pub mod serde_msgpack {
        use crate::*;
        use p2p_client::network::NetworkMessage;

        fn bench_s11n(c: &mut Criterion, size: usize) {
            let bench_id = format!("Serde msgpack serialization with {}B messages", size);

            let mut msg = create_random_packet(size);
            let mut buffer = Cursor::new(Vec::with_capacity(size));

            c.bench_function(&bench_id, move |b| {
                b.iter(|| {
                    msg.rewind_packet();
                    msg.serialize(&mut buffer).unwrap();
                    NetworkMessage::deserialize(&buffer.get_ref()).unwrap();
                    buffer.seek(SeekFrom::Start(0)).unwrap();
                })
            });
        }

        pub fn bench_s11n_256(b: &mut Criterion) { bench_s11n(b, 256) }
        pub fn bench_s11n_1k(b: &mut Criterion) { bench_s11n(b, 1024) }
        pub fn bench_s11n_4k(b: &mut Criterion) { bench_s11n(b, 4096) }
        pub fn bench_s11n_64k(b: &mut Criterion) { bench_s11n(b, 64 * 1024) }
        pub fn bench_s11n_256k(b: &mut Criterion) { bench_s11n(b, 256 * 1024) }
        pub fn bench_s11n_1m(b: &mut Criterion) { bench_s11n(b, 1024 * 1024) }
        pub fn bench_s11n_4m(b: &mut Criterion) { bench_s11n(b, 4 * 1024 * 1024) }
    }

    #[cfg(feature = "s11n_capnp")]
    pub mod capnp {
        use crate::*;
        use p2p_client::network::serialization::cap::{deserialize, serialize};

        fn bench_s11n(c: &mut Criterion, size: usize, packed: bool) {
            let bench_id = format!(
                "CAPnP (packed: {}) serialization with {}B messages",
                packed, size
            );

            let mut msg = create_random_packet(size);
            let mut buffer = Cursor::new(Vec::with_capacity(size));

            c.bench_function(&bench_id, move |b| {
                b.iter(|| {
                    msg.rewind_packet();
                    serialize(&mut buffer, &mut msg, packed).unwrap();
                    buffer.seek(SeekFrom::Start(0)).unwrap();
                    deserialize(&mut buffer, packed)
                })
            });
        }

        pub fn bench_s11n_256(b: &mut Criterion) { bench_s11n(b, 256, false) }
        pub fn bench_s11n_1k(b: &mut Criterion) { bench_s11n(b, 1024, false) }
        pub fn bench_s11n_4k(b: &mut Criterion) { bench_s11n(b, 4096, false) }
        pub fn bench_s11n_64k(b: &mut Criterion) { bench_s11n(b, 64 * 1024, false) }
        pub fn bench_s11n_256k(b: &mut Criterion) { bench_s11n(b, 256 * 1024, false) }
        pub fn bench_s11n_1m(b: &mut Criterion) { bench_s11n(b, 1024 * 1024, false) }
        pub fn bench_s11n_4m(b: &mut Criterion) { bench_s11n(b, 4 * 1024 * 1024, false) }

        pub fn bench_s11n_256_packed(b: &mut Criterion) { bench_s11n(b, 256, true) }
        pub fn bench_s11n_1k_packed(b: &mut Criterion) { bench_s11n(b, 1024, true) }
        pub fn bench_s11n_4k_packed(b: &mut Criterion) { bench_s11n(b, 4096, true) }
        pub fn bench_s11n_64k_packed(b: &mut Criterion) { bench_s11n(b, 64 * 1024, true) }
        pub fn bench_s11n_256k_packed(b: &mut Criterion) { bench_s11n(b, 256 * 1024, true) }
        pub fn bench_s11n_1m_packed(b: &mut Criterion) { bench_s11n(b, 1024 * 1024, true) }
        pub fn bench_s11n_4m_packed(b: &mut Criterion) { bench_s11n(b, 4 * 1024 * 1024, true) }
    }
}

criterion_group!(
    dedup_benches,
    deduplication::bench_dedup_1k,
    deduplication::bench_dedup_4k,
    deduplication::bench_dedup_16k,
    deduplication::bench_dedup_32k
);

criterion_group!(
    s11n_fbs_benches,
    s11n::fbs::bench_s11n_256,
    s11n::fbs::bench_s11n_1k,
    s11n::fbs::bench_s11n_4k,
    s11n::fbs::bench_s11n_64k,
    s11n::fbs::bench_s11n_256k,
    s11n::fbs::bench_s11n_1m,
    s11n::fbs::bench_s11n_4m,
);

#[cfg(feature = "s11n_capnp")]
criterion_group!(
    s11n_capnp_benches,
    s11n::capnp::bench_s11n_256,
    s11n::capnp::bench_s11n_1k,
    s11n::capnp::bench_s11n_4k,
    s11n::capnp::bench_s11n_64k,
    s11n::capnp::bench_s11n_256k,
    s11n::capnp::bench_s11n_1m,
    s11n::capnp::bench_s11n_4m,
    s11n::capnp::bench_s11n_256_packed,
    s11n::capnp::bench_s11n_1k_packed,
    s11n::capnp::bench_s11n_4k_packed,
    s11n::capnp::bench_s11n_64k_packed,
    s11n::capnp::bench_s11n_256k_packed,
    s11n::capnp::bench_s11n_1m_packed,
    s11n::capnp::bench_s11n_4m_packed,
);
#[cfg(not(feature = "s11n_capnp"))]
criterion_group!(s11n_capnp_benches, common::nop_bench);

#[cfg(feature = "s11n_serde_cbor")]
criterion_group!(
    s11n_cbor_benches,
    s11n::serde_cbor::bench_s11n_256,
    s11n::serde_cbor::bench_s11n_1k,
    s11n::serde_cbor::bench_s11n_4k,
    s11n::serde_cbor::bench_s11n_64k,
    s11n::serde_cbor::bench_s11n_256k,
    s11n::serde_cbor::bench_s11n_1m,
    s11n::serde_cbor::bench_s11n_4m,
);
#[cfg(not(feature = "s11n_serde_cbor"))]
criterion_group!(s11n_cbor_benches, common::nop_bench);

#[cfg(feature = "s11n_serde_msgpack")]
criterion_group!(
    s11n_msgpack_benches,
    s11n::serde_msgpack::bench_s11n_256,
    s11n::serde_msgpack::bench_s11n_1k,
    s11n::serde_msgpack::bench_s11n_4k,
    s11n::serde_msgpack::bench_s11n_64k,
    s11n::serde_msgpack::bench_s11n_256k,
    s11n::serde_msgpack::bench_s11n_1m,
    s11n::serde_msgpack::bench_s11n_4m,
);
#[cfg(not(feature = "s11n_serde_msgpack"))]
criterion_group!(s11n_msgpack_benches, common::nop_bench);

criterion_main!(
    s11n_fbs_benches,
    s11n_capnp_benches,
    s11n_cbor_benches,
    s11n_msgpack_benches,
    dedup_benches,
);
