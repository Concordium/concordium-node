#[macro_use]
extern crate criterion;

use p2p_client::test_utils::{create_random_packet, generate_random_data};

use std::{
    io::{Cursor, Seek, SeekFrom},
    time::Duration,
};

#[cfg(any(
    not(feature = "s11n_fbs"),
    not(feature = "s11n_capnp"),
    not(feature = "s11n_serde")
))]
mod nop {
    use criterion::Criterion;
    pub fn nop_bench(_c: &mut Criterion) {}
}

macro_rules! bench_s11n {
    ($name:expr) => {
        use crate::*;
        use criterion::{BenchmarkId, Criterion, Throughput};
        use p2p_client::network::NetworkMessage;

        pub fn bench_s11n(c: &mut Criterion) {
            let mut group = c.benchmark_group($name);

            for &size in &[
                256,
                1024,
                4096,
                64 * 1024,
                256 * 1024,
                1024 * 1024,
                4 * 1024 * 1024,
            ] {
                let mut msg = create_random_packet(size);
                let mut buffer = Cursor::new(Vec::with_capacity(size));

                group.throughput(Throughput::Bytes(size as u64));
                group.bench_function(BenchmarkId::from_parameter(size), |b| {
                    b.iter(|| {
                        msg.rewind_packet();
                        msg.serialize(&mut buffer).unwrap();
                        NetworkMessage::deserialize(&buffer.get_ref()).unwrap();
                        buffer.seek(SeekFrom::Start(0)).unwrap();
                    })
                });
            }
            group.finish();
        }
    };
}

mod dedup {
    use crate::*;
    use circular_queue::CircularQueue;
    use criterion::{BenchmarkId, Criterion, Throughput};
    use digest::Digest;
    use twox_hash::XxHash64;

    pub fn bench_dedup(c: &mut Criterion) {
        const MSG_SIZE: usize = 250;
        let mut group = c.benchmark_group("dedup queue with 250B messages");
        for &size in &[1024, 4096, 1024 * 16, 1024 * 32] {
            let mut queue = CircularQueue::with_capacity(size);
            for _ in 0..size {
                let mut msg_hash = [0u8; 8];
                msg_hash.copy_from_slice(&XxHash64::digest(&generate_random_data(MSG_SIZE)));
                queue.push(msg_hash);
            }

            group.throughput(Throughput::Elements(size as u64));
            group.bench_function(BenchmarkId::from_parameter(size), |b| {
                b.iter(|| {
                    let new_msg = generate_random_data(250);
                    let mut new_msg_hash = [0u8; 8];
                    new_msg_hash.copy_from_slice(&XxHash64::digest(&new_msg));

                    if !queue.iter().any(|h| h == &new_msg_hash) {
                        queue.push(new_msg_hash);
                    }
                })
            });
        }
        group.finish();
    }
}

mod s11n {
    #[cfg(feature = "s11n_fbs")]
    pub mod fbs {
        bench_s11n!("flatbuffers");
    }

    #[cfg(feature = "s11n_capnp")]
    pub mod capnp {
        bench_s11n!("capnproto");
    }

    #[cfg(feature = "s11n_serde_cbor")]
    pub mod cbor {
        bench_s11n!("CBOR");
    }

    #[cfg(feature = "s11n_serde_msgpack")]
    pub mod msgpack {
        bench_s11n!("msgpack");
    }
}

#[cfg(feature = "s11n_fbs")]
criterion_group!(s11n_fbs_benches, s11n::fbs::bench_s11n);
#[cfg(not(feature = "s11n_fbs"))]
criterion_group!(s11n_fbs_benches, nop::nop_bench);

#[cfg(feature = "s11n_capnp")]
criterion_group!(s11n_capnp_benches, s11n::capnp::bench_s11n);
#[cfg(not(feature = "s11n_capnp"))]
criterion_group!(s11n_capnp_benches, nop::nop_bench);

#[cfg(feature = "s11n_serde_cbor")]
criterion_group!(s11n_cbor_benches, s11n::cbor::bench_s11n);
#[cfg(not(feature = "s11n_serde_cbor"))]
criterion_group!(s11n_cbor_benches, nop::nop_bench);

#[cfg(feature = "s11n_serde_msgpack")]
criterion_group!(s11n_msgpack_benches, s11n::msgpack::bench_s11n);
#[cfg(not(feature = "s11n_serde_msgpack"))]
criterion_group!(s11n_msgpack_benches, nop::nop_bench);

criterion_group!(dedup_benches, dedup::bench_dedup);

criterion_main!(
    s11n_fbs_benches,
    s11n_capnp_benches,
    s11n_cbor_benches,
    s11n_msgpack_benches,
    dedup_benches,
);
