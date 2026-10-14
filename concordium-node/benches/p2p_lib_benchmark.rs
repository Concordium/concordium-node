#[macro_use]
extern crate criterion;

macro_rules! bench_s11n {
    ($name:expr) => {
        use concordium_node::{network::NetworkMessage, test_utils::create_random_packet};
        use criterion::{BenchmarkId, Criterion, Throughput};
        use std::io::{Cursor, Seek, SeekFrom};

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

mod s11n {
    pub mod fbs {
        bench_s11n!("flatbuffers");
    }
}

criterion_group!(s11n_fbs_benches, s11n::fbs::bench_s11n);

criterion_main!(s11n_fbs_benches);
