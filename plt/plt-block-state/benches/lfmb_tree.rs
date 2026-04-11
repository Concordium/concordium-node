//! Benchmarks for the [`LFMBTree`] covering asymptotic behavior with varying tree size.

use divan::counter::ItemsCount;
use divan::{AllocProfiler, Bencher};
use plt_block_state::block_state::blob_store::StoreSerialized;
use plt_block_state::block_state::blob_store::test_stub::{BlobStoreStub, UnreachableBlobStore};
use plt_block_state::block_state::lfmb_tree::{LFMBTree, LFMBTreeKey};
use std::time::Duration;

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

const SIZES: &[u64] = &[(1 << 10) - 1, (1 << 15) - 1, (1 << 20) - 1];

fn build_tree(size: u64) -> BenchTree {
    let mut tree = BenchTree::empty();
    for i in 0..size {
        (_, tree) = tree
            .insert_value(&mut UnreachableBlobStore, StoreSerialized(i))
            .unwrap();
    }
    tree
}

// /// Warmup CPU. Divan does not seem to be good at doing this automatically.
// #[divan::bench]
// fn a_warmup() {
//     let mut x: u64 = 0;
//     for i in divan::black_box(0..10_000_000) {
//         x = x.wrapping_add(i);
//     }
//     divan::black_box(x);
// }

#[divan::bench(args = SIZES)]
fn bench_with_value(bencher: Bencher, size: u64) {
    let tree = divan::black_box(build_tree(size));
    bencher
        .with_inputs(|| BenchKey(rand::random_range(0..size)))
        .bench_local_values(|key| {
            tree.with_value(&UnreachableBlobStore, key, |v| Ok(*v))
                .unwrap()
                .unwrap()
        });
}

#[divan::bench(args = SIZES)]
fn bench_insert_value(bencher: Bencher, size: u64) {
    let tree = divan::black_box(build_tree(size));
    bencher.bench_local(|| {
        tree.insert_value(&UnreachableBlobStore, StoreSerialized(0))
            .unwrap()
    });
}

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

#[divan::bench(args = SIZES)]
fn bench_values(bencher: Bencher, size: u64) {
    let tree = divan::black_box(build_tree(size));
    bencher.bench_local(|| {
        tree.values(&UnreachableBlobStore, |v| Ok(v.0))
            .map(|r| r.unwrap())
            .sum::<u64>()
    });
}
