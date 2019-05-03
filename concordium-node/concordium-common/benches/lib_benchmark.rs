#[macro_use]
extern crate criterion;

mod common {
    pub mod ucursor {
        use concordium_common::{ContainerView, UCursor};

        use criterion::Criterion;
        use rand::{distributions::Standard, thread_rng, Rng};

        fn make_content_with_size(content_size: usize) -> Vec<u8> {
            thread_rng()
                .sample_iter(&Standard)
                .take(content_size)
                .collect::<Vec<u8>>()
        }

        pub fn from_memory_to_file_1m(b: &mut Criterion) { from_memory_to_file(1024 * 1024, b) }

        pub fn from_memory_to_file_4m(b: &mut Criterion) { from_memory_to_file(4 * 1024 * 1024, b) }

        pub fn from_memory_to_file_32m(b: &mut Criterion) {
            from_memory_to_file(32 * 1024 * 1024, b)
        }

        fn from_memory_to_file(content_size: usize, c: &mut Criterion) {
            let content = make_content_with_size(content_size);
            let view = ContainerView::from(content);
            let bench_id = format!("Benchmark from memory to file using {} bytes", content_size);

            c.bench_function(bench_id.as_str(), move |b| {
                let cloned_view = view.clone();
                b.iter(|| {
                    let mut cur = UCursor::build_from_view(cloned_view.clone());
                    cur.swap_to_file()
                })
            });
        }
    }
}

criterion_group!(
    ucursor_benches,
    common::ucursor::from_memory_to_file_1m,
    common::ucursor::from_memory_to_file_4m,
    common::ucursor::from_memory_to_file_32m
);

criterion_main!(ucursor_benches,);
