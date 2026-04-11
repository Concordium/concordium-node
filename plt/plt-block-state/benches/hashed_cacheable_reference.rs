use divan::counter::ItemsCount;
use divan::{AllocProfiler, Bencher};
use plt_block_state::block_state::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use plt_block_state::block_state::blob_store::StoreSerialized;
use plt_block_state::block_state::blob_store::test_stub::{BlobStoreStub, UnreachableBlobStore};
use plt_block_state::block_state::lfmb_tree::{LFMBTree, LFMBTreeKey};
use std::time::Duration;

fn main() {
    divan::main();
}

const THREADS: &[usize] = &[1, 2, 4, 8, 16];

#[divan::bench(threads = THREADS)]
fn bench_with_value(bencher: Bencher) {
    let hcr = divan::black_box(HashedCacheableRef::new(StoreSerialized(0)));
    bencher.bench(|| hcr.with_value(&UnreachableBlobStore, |v| Ok(*v)).unwrap());
}
