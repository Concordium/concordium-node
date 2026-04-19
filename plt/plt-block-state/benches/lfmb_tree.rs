//! Benchmarks for the [`LfmbTree`] covering asymptotic behavior with varying tree size.

use divan::Bencher;
use plt_block_state::block_state::blob_store;
use plt_block_state::block_state::blob_store::StoreSerialized;
use plt_block_state::block_state::blob_store::test_stub::{BlobStoreStub, UnreachableBlobStore};
use plt_block_state::block_state::cacheable::Cacheable;
use plt_block_state::block_state::hash::Hashable;
use plt_block_state::block_state::lfmb_tree::{LfmbTree, LfmbTreeKey};

fn main() {
    divan::main();
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct BenchKey(u64);

impl LfmbTreeKey for BenchKey {
    fn to_u64(self) -> u64 {
        self.0
    }

    fn from_u64(key: u64) -> Self {
        Self(key)
    }
}

type BenchTree = LfmbTree<BenchKey, StoreSerialized<u64>>;

/// Sizes of trees to benchmark operations on.
const SIZES: &[u64] = &[(1 << 10) - 1, (1 << 15) - 1, (1 << 20) - 1];

/// Small sizes of trees to benchmark operations on. Used for benchmarks that needs
/// to create the full tree for each iteration.
const SMALL_SIZES: &[u64] = &[1 << 5, 1 << 10, 1 << 15];

fn build_tree(size: u64) -> BenchTree {
    let mut tree = BenchTree::empty();
    for i in 0..size {
        (_, tree) = tree
            .insert_value(&UnreachableBlobStore, StoreSerialized(i))
            .unwrap();
    }
    tree
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

/// Benchmark [`LfmbTree::lookup_value`] for different tree sizes.
#[divan::bench(args = SIZES)]
fn bench_lookup_value(bencher: Bencher, size: u64) {
    let tree = divan::black_box(build_tree(size));
    bencher
        .with_inputs(|| BenchKey(rand::random_range(0..size)))
        .bench_local_values(|key| {
            tree.lookup_value(&UnreachableBlobStore, key)
                .unwrap()
                .unwrap()
        });
}

/// Benchmark [`LfmbTree::insert_value`] by creating trees of different sizes.
#[divan::bench(args = SMALL_SIZES)]
fn bench_insert_values(bencher: Bencher, size: u64) {
    bencher.bench_local(|| build_tree(size));
}

/// Benchmark [`LfmbTree::update_value`] for different tree sizes.
#[divan::bench(args = SIZES)]
fn bench_update_value(bencher: Bencher, size: u64) {
    let tree = divan::black_box(build_tree(size));
    bencher
        .with_inputs(|| BenchKey(rand::random_range(0..size)))
        .bench_local_values(|key| {
            tree.update_value(&UnreachableBlobStore, key, |v| Ok(StoreSerialized(v.0 + 1)))
                .unwrap()
                .unwrap()
        });
}

/// Benchmark [`LfmbTree::values`] iterator for different tree sizes.
#[divan::bench(args = SIZES)]
fn bench_values(bencher: Bencher, size: u64) {
    let tree = divan::black_box(build_tree(size));
    bencher.bench_local(|| {
        tree.values(&UnreachableBlobStore, |_k, v| Ok(v.0))
            .map(|r| r.unwrap())
            .sum::<u64>()
    });
}

/// Benchmark [`LfmbTree::hash`] iterator for different tree sizes.
#[divan::bench(args = SMALL_SIZES)]
fn bench_hash(bencher: Bencher, size: u64) {
    bencher
        .with_inputs(|| build_tree(size))
        .bench_local_values(|tree| tree.hash(&UnreachableBlobStore).unwrap());
}

/// Benchmark [`LfmbTree::hash`] iterator for different tree sizes.
#[divan::bench(args = SMALL_SIZES)]
fn bench_store(bencher: Bencher, size: u64) {
    bencher
        .with_inputs(|| build_tree(size))
        .bench_local_values(|tree| blob_store::store_to_store(&mut BlobStoreStub::default(), tree));
}

/// Benchmark [`LfmbTree::hash`] iterator for different tree sizes.
#[divan::bench(args = SMALL_SIZES)]
fn bench_cache(bencher: Bencher, size: u64) {
    let mut store = BlobStoreStub::default();
    let tree = build_tree(size);
    let blob_ref = divan::black_box(blob_store::store_to_store(&mut store, tree));

    bencher
        .with_inputs(|| {
            let tree: BenchTree = blob_store::load_from_store(&store, blob_ref).unwrap();
            tree
        })
        .bench_local_values(|tree| tree.cache_reference_values(&store));
}
