#[macro_use]
extern crate criterion;

mod nop {
    use criterion::Criterion;
    pub fn nop_bench(_c: &mut Criterion) {}
}

macro_rules! bench_s11n {
    ($name:expr) => {
        use concordium_node::{network::NetworkMessage, test_utils::create_random_packet};
        use criterion::{BenchmarkId, Criterion, Throughput};
        use std::io::{Cursor, Seek, SeekFrom};

        pub fn bench_s11n(c: &mut Criterion) {
            let mut group = c.benchmark_group($name);

            for &size in &[256, 1024, 4096, 64 * 1024, 256 * 1024, 1024 * 1024, 4 * 1024 * 1024] {
                let msg = create_random_packet(size);
                let mut buffer = Cursor::new(Vec::with_capacity(size));

                group.throughput(Throughput::Bytes(size as u64));
                group.bench_function(BenchmarkId::from_parameter(size), |b| {
                    b.iter(|| {
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

#[cfg(feature = "dedup_benchmarks")]
macro_rules! dedup_bench {
    ($f:ident, $hasher:ty, $hasher_name:expr, $hash_size:expr, $msg_size:expr) => {
        pub fn $f(c: &mut Criterion) {
            const MSG_SIZE: usize = $msg_size;
            let mut group = c.benchmark_group(format!(
                "{} dedup queue with {} B messages",
                $hasher_name, $msg_size
            ));
            for &size in &[1024, 4096, 1024 * 16, 1024 * 32] {
                let mut queue = CircularQueue::with_capacity(size);
                for _ in 0..size {
                    let mut msg_hash = [0u8; $hash_size];
                    msg_hash.copy_from_slice(&<$hasher>::digest(&generate_random_data(MSG_SIZE)));
                    queue.push(msg_hash);
                }

                if MSG_SIZE > 4_000_000 {
                    group.measurement_time(Duration::from_secs(240));
                } else if MSG_SIZE > 1_000_000 {
                    group.measurement_time(Duration::from_secs(60));
                }

                group.throughput(Throughput::Elements(size as u64));
                group.bench_function(BenchmarkId::from_parameter(size), |b| {
                    b.iter(|| {
                        let new_msg = generate_random_data($msg_size);
                        let mut new_msg_hash = [0u8; $hash_size];
                        new_msg_hash.copy_from_slice(&<$hasher>::digest(&new_msg));

                        if !queue.iter().any(|h| h == &new_msg_hash) {
                            queue.push(new_msg_hash);
                        }
                    })
                });
            }
            group.finish();
        }
    };
}

#[cfg(feature = "dedup_benchmarks")]
mod dedup {
    use circular_queue::CircularQueue;
    use concordium_node::test_utils::generate_random_data;
    use criterion::{BenchmarkId, Criterion, Throughput};
    use digest::Digest;
    use sha2::Sha256;
    use std::time::Duration;
    use twox_hash::XxHash64;

    dedup_bench!(small_bench_dedup_xxhash64, XxHash64, "XxHash64", 8, 250);
    dedup_bench!(small_bench_dedup_sha256, Sha256, "SHA256", 32, 250);
    dedup_bench!(medium_bench_dedup_xxhash64, XxHash64, "XxHash64", 8, 1_048_576);
    dedup_bench!(medium_bench_dedup_sha256, Sha256, "SHA256", 32, 1_048_576);
    dedup_bench!(big_bench_dedup_xxhash64, XxHash64, "XxHash64", 8, 4_194_304);
    dedup_bench!(big_bench_dedup_sha256, Sha256, "SHA256", 32, 4_194_304);
}

mod s11n {
    pub mod fbs {
        bench_s11n!("flatbuffers");
    }
}

criterion_group!(s11n_fbs_benches, s11n::fbs::bench_s11n);

#[cfg(feature = "dedup_benchmarks")]
criterion_group!(
    dedup_benches,
    dedup::small_bench_dedup_xxhash64,
    dedup::small_bench_dedup_sha256,
    dedup::medium_bench_dedup_xxhash64,
    dedup::medium_bench_dedup_sha256,
    dedup::big_bench_dedup_xxhash64,
    dedup::big_bench_dedup_sha256
);
#[cfg(not(feature = "dedup_benchmarks"))]
criterion_group!(dedup_benches, nop::nop_bench);

criterion_main!(s11n_fbs_benches, dedup_benches,);
