//! Benchmarks for the [`LFMBTree`] covering asymptotic behavior with varying tree size.

use divan::Bencher;
use plt_block_state::block_state::blob_store::StoreSerialized;
use plt_block_state::block_state::blob_store::test_stub::BlobStoreStub;
use plt_block_state::block_state::lfmb_tree::{LFMBTree, LFMBTreeKey};
use std::hint;

fn main() {
    divan::main();
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct BenchKey(u64);

impl LFMBTreeKey for BenchKey {
    fn to_u64(self) -> u64 {
        self.0
    }

    fn from_u64(key: u64) -> Self {
        Self(key)
    }
}

type BenchTree = LFMBTree<BenchKey, StoreSerialized<u64>>;

const SIZES: &[u64] = &[1 << 10, 1 << 15, 1 << 20];

/// Build a tree of the given `size` using a fresh in-memory blob store.
fn build_tree(size: u64) -> (BlobStoreStub, BenchTree) {
    let mut store = BlobStoreStub::default();
    let mut tree = BenchTree::empty();
    for i in 0..size {
        (_, tree) = tree.insert_value(&mut store, StoreSerialized(i)).unwrap();
    }
    (store, tree)
}

/// Benchmark `with_value` (point lookup) – expected O(log n) per call.
/// We look up the *last* key so the traversal always reaches the deepest path.
#[divan::bench(args = SIZES)]
fn bench_with_value(bencher: Bencher, size: u64) {
    let (store, tree) = build_tree(size);
    let last_key = BenchKey(size - 1);

    bencher.bench(|| {
        divan::black_box(&tree)
            .with_value(&store, hint::black_box(last_key), |v| Ok(*v))
            .unwrap()
            .unwrap()
    });
}

/// Benchmark `insert_value` – expected O(log n) per insertion.
/// We measure the cost of inserting one more element into a tree of the given
/// size.
#[divan::bench(args = SIZES)]
fn bench_insert_value(bencher: Bencher, size: u64) {
    let (store, tree) = build_tree(size);

    bencher.bench(|| {
        divan::black_box(&tree)
            .insert_value(&store, StoreSerialized(size))
            .unwrap()
    });
}

/// Benchmark `update_value` – expected O(log n) per update.
/// We update the *last* key (deepest path).
#[divan::bench(args = SIZES)]
fn bench_update_value(bencher: Bencher, size: u64) {
    let (store, tree) = build_tree(size);
    let last_key = BenchKey(size - 1);

    bencher.bench(|| {
        divan::black_box(&tree)
            .update_value(&store, divan::black_box(last_key), |v| {
                Ok(StoreSerialized(v.0 + 1))
            })
            .unwrap()
            .unwrap()
    });
}

/// Benchmark `values` (full iteration) – expected O(n).
/// We consume the entire iterator and sum all values.
#[divan::bench(args = SIZES)]
fn bench_values(bencher: Bencher, size: u64) {
    let (store, tree) = build_tree(size);

    bencher.bench(|| {
        divan::black_box(&tree)
            .values(&store, |v| Ok(v.0))
            .map(|r| r.unwrap())
            .sum::<u64>()
    });
}
