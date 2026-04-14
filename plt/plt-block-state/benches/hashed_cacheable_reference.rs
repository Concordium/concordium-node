//! Benchmarks for the [`HashedCacheableRef`] covering congestion by concurrency.

use divan::Bencher;
use plt_block_state::block_state::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use plt_block_state::block_state::blob_store::StoreSerialized;
use plt_block_state::block_state::blob_store::test_stub::UnreachableBlobStore;

fn main() {
    divan::main();
}

const THREADS: &[usize] = &[1, 2, 4, 8, 16];

/// Warmup CPU. Divan does not seem to be good at doing this automatically.
#[divan::bench]
fn a_warmup() {
    let mut x: u64 = 0;
    for i in divan::black_box(0..10_000_000) {
        x = x.wrapping_add(i);
    }
    divan::black_box(x);
}

/// Benchmark [`HashedCacheableRef::with_value`] for using different number of concurrent threads.
#[divan::bench(threads = THREADS)]
fn bench_with_value(bencher: Bencher) {
    let hcr = divan::black_box(HashedCacheableRef::new(StoreSerialized(0)));
    bencher.bench(|| hcr.value(&UnreachableBlobStore).unwrap());
}
