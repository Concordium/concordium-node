//! Benchmarks for the [`Trie`] covering asymptotic behavior with varying trie size.

use divan::Bencher;
use plt_block_state::persistent::blob_store;
use plt_block_state::persistent::blob_store::StoreSerialized;
use plt_block_state::persistent::blob_store::test_stub::{BlobStoreStub, UnreachableBlobStore};
use plt_block_state::persistent::cacheable::Cacheable;
use plt_block_state::persistent::hash::Hashable;
use plt_block_state::persistent::trie::Trie;
use tinyvec::{TinyVec, tiny_vec};

fn main() {
    divan::main();
}

type BenchTrie = Trie<8, TinyVec<[u8; 8]>, StoreSerialized<u64>>;

/// Sizes of tries to benchmark operations on.
const SIZES_LOG: &[u64] = &[1 << 10, 1 << 15, 1 << 20];

/// Small sizes of tries to benchmark operations on. Used for benchmarks that needs
/// to create the full trie for each iteration.
const SMALL_SIZES_LOG: &[u64] = &[1 << 5, 1 << 10, 1 << 15];

fn generate_key(trie_size: u64) -> TinyVec<[u8; 8]> {
    // Double range compared to size to not fill trie completely
    rand::random_range(0..trie_size * 2).to_be_bytes().into()
}

fn build_trie(size: u64) -> BenchTrie {
    let mut trie = BenchTrie::empty();
    for i in 0..size {
        trie = trie
            .insert_or_update_entry(
                &UnreachableBlobStore,
                &generate_key(size),
                StoreSerialized(i),
            )
            .unwrap();
    }
    trie
}

/// Warmup CPU. Divan does not seem to be good at doing this automatically.
#[divan::bench]
fn a_warmup() {
    let mut x: u64 = 0;
    for i in divan::black_box(0..10_000_000) {
        x = x.wrapping_add(i);
    }
    divan::black_box(x);
}

/// Benchmark [`Trie::lookup_value`] for different trie sizes.
#[divan::bench(args = SIZES_LOG)]
fn bench_lookup_value(bencher: Bencher, size: u64) {
    let trie = divan::black_box(build_trie(size));
    bencher
        .with_inputs(|| generate_key(size))
        .bench_local_values(|key| trie.lookup_value(&UnreachableBlobStore, &key).unwrap());
}

/// Benchmark [`Trie::contains_key`] for different trie sizes.
#[divan::bench(args = SIZES_LOG)]
fn bench_contains_key(bencher: Bencher, size: u64) {
    let trie = divan::black_box(build_trie(size));
    bencher
        .with_inputs(|| generate_key(size))
        .bench_local_values(|key| trie.contains_key(&UnreachableBlobStore, &key).unwrap());
}

/// Benchmark [`Trie::insert_or_update_entry`] by creating tries of different sizes.
#[divan::bench(args = SMALL_SIZES_LOG)]
fn bench_build_trie(bencher: Bencher, size: u64) {
    bencher.bench_local(|| build_trie(size));
}

/// Benchmark [`Trie::insert_or_update_entry`] for different trie sizes.
#[divan::bench(args = SIZES_LOG)]
fn bench_insert_or_update_entry(bencher: Bencher, size: u64) {
    let trie = divan::black_box(build_trie(size));
    bencher
        .with_inputs(|| generate_key(size))
        .bench_local_values(|key| {
            trie.insert_or_update_entry(&UnreachableBlobStore, &key, StoreSerialized(0))
                .unwrap()
        });
}

/// Benchmark [`Trie::delete_entry`] for different trie sizes.
#[divan::bench(args = SIZES_LOG)]
fn bench_delete_entry(bencher: Bencher, size: u64) {
    let trie = divan::black_box(build_trie(size));
    bencher
        .with_inputs(|| generate_key(size))
        .bench_local_values(|key| trie.delete_entry(&UnreachableBlobStore, &key).unwrap());
}

/// Benchmark [`Trie::iter_prefix`] iterator by iterating full trie for different trie sizes.
#[divan::bench(args = SIZES_LOG)]
fn bench_iter_full_trie(bencher: Bencher, size: u64) {
    let trie = divan::black_box(build_trie(size));
    bencher.bench_local(|| {
        trie.iter_prefix(&UnreachableBlobStore, &tiny_vec![])
            .unwrap()
            .map(|r| r.unwrap().1.0)
            .sum::<u64>()
    });
}

/// Benchmark [`Trie::hash`] iterator for different trie sizes.
#[divan::bench(args = SMALL_SIZES_LOG)]
fn bench_hash(bencher: Bencher, size: u64) {
    bencher
        .with_inputs(|| build_trie(size))
        .bench_local_values(|trie| trie.hash(&UnreachableBlobStore).unwrap());
}

/// Benchmark [`Trie::store_to_buffer`] for different trie sizes.
#[divan::bench(args = SMALL_SIZES_LOG)]
fn bench_store_load(bencher: Bencher, size: u64) {
    bencher
        .with_inputs(|| build_trie(size))
        .bench_local_values(|trie| {
            let mut store = BlobStoreStub::default();
            let location = blob_store::store_to_store(&mut store, trie);
            blob_store::load_from_store::<BenchTrie>(&store, location).unwrap()
        });
}

/// Benchmark [`Trie::cache_reference_values`] for different trie sizes.
#[divan::bench(args = SMALL_SIZES_LOG)]
fn bench_cache(bencher: Bencher, size: u64) {
    let mut store = BlobStoreStub::default();
    let trie = build_trie(size);
    let blob_ref = divan::black_box(blob_store::store_to_store(&mut store, trie));

    bencher
        .with_inputs(|| {
            let trie: BenchTrie = blob_store::load_from_store(&store, blob_ref).unwrap();
            trie
        })
        .bench_local_values(|trie| trie.cache_reference_values(&store));
}
