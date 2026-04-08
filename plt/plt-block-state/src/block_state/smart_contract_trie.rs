//! Adapter for the trie in the `concordium-smart-contract-engine` crate. The traits
//! and methodology does not match 1-1, hence this adapter is needed to use it in the
//! block state.

use crate::block_state::blob_reference::BlobStoreLocation;
use crate::block_state::blob_store::{BlobStoreLoad, BlobStoreStore, Loadable, Storable};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use crate::block_state_interface::{BlockStateError, BlockStateResult};
use concordium_base::common::Buffer;
use concordium_base::hashes::Hash;
use concordium_smart_contract_engine::v1::trie;
use concordium_smart_contract_engine::v1::trie::{LoadResult, Reference};
use std::io::Read;
use std::sync::RwLock;

#[derive(Debug)]
pub struct PersistentState(RwLock<trie::PersistentState>);

impl PersistentState {
    pub fn empty() -> Self {
        Self(RwLock::new(trie::PersistentState::Empty))
    }
}

#[derive(Debug)]
pub struct MutableState(trie::MutableState);

struct StorerAdapter<'a, S>(&'a mut S);

impl<'a, S: BlobStoreStore> trie::BackingStoreStore for StorerAdapter<'a, S> {
    fn store_raw(&mut self, data: &[u8]) -> Result<trie::Reference, trie::WriteError> {
        let location = self.0.store_raw(data);
        Ok(trie::Reference {
            reference: location.0,
        })
    }
}

impl Storable for PersistentState {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        let mut mut_persistent_state = self.0.write().expect("PersistentState lock poisoned");
        mut_persistent_state
            .store_update_buf(&mut StorerAdapter(storer), &mut buffer)
            .expect("error writing PersistentState to blob store");
    }
}

struct LoaderAdapter<'a, L>(&'a L);

impl<'a, L: BlobStoreLoad> trie::BackingStoreLoad for LoaderAdapter<'a, L> {
    type R = Vec<u8>;

    fn load_raw(&mut self, location: Reference) -> LoadResult<Self::R> {
        Ok(self.0.load_raw(BlobStoreLocation(location.reference)))
    }
}

impl Loadable for PersistentState {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> Result<Self, BlockStateError> {
        let persistent_state = <trie::PersistentState as trie::Loadable>::load(
            &mut LoaderAdapter(loader),
            &mut buffer,
        )
        .map_err(|load_err| {
            BlockStateError::BlobStoreDecode(format!("Error loading PersistentState: {}", load_err))
        })?;
        Ok(PersistentState(RwLock::new(persistent_state)))
    }
}

impl Cacheable for PersistentState {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        let mut mut_persistent_state = self.0.write().expect("PersistentState lock poisoned");
        mut_persistent_state.cache(&mut LoaderAdapter(loader));
        Ok(())
    }
}

impl Hashable for PersistentState {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        let mut mut_persistent_state = self.0.write().expect("PersistentState lock poisoned");
        Ok(Hash::from(
            mut_persistent_state.hash(&mut LoaderAdapter(loader)).hash,
        ))
    }
}
