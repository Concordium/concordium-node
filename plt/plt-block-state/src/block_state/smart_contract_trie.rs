//! Adapter for the trie in the `concordium-smart-contract-engine` crate. The traits
//! and methodology does not match 1-1, hence this adapter is needed to use it in the
//! block state.

use crate::block_state::blob_store::{
    BlobStoreLoad, BlobStoreLocation, BlobStoreStore, Loadable, Storable,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use crate::block_state_interface::{BlockStateError, BlockStateResult};
use concordium_base::common::Buffer;
use concordium_base::hashes::Hash;
use concordium_smart_contract_engine::v1::trie;
use concordium_smart_contract_engine::v1::trie::{EntryId, NoError};
use std::io::Read;
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

#[derive(Debug)]
pub struct PersistentState(RwLock<trie::PersistentState>);

impl PersistentState {
    pub fn empty() -> Self {
        Self(RwLock::new(trie::PersistentState::Empty))
    }

    pub fn lookup_value(&self, loader: &impl BlobStoreLoad, key: &[u8]) -> Option<Vec<u8>> {
        let mut loader = LoaderAdapter(loader);
        let mut persistent_state = self.lock_read();
        persistent_state.lookup(&mut loader, key)
    }

    pub fn iter_prefix<'a, L: BlobStoreLoad>(
        &self,
        loader: &'a L,
        prefix: &[u8],
    ) -> BlockStateResult<impl Iterator<Item = (Vec<u8>, Vec<u8>)> + use<'a, L>> {
        let mut loader_adapter = LoaderAdapter(loader);
        let mut mutable_state = self.lock_read().thaw();
        let mut trie = mutable_state.get_inner(&mut loader_adapter).lock();
        let trie_iter = trie.iter(&mut loader_adapter, prefix).map_err(|err| {
            BlockStateError::Invariant(format!("Error iterating values in MutableTrie: {}", err))
        })?;

        Ok(PrefixIterator {
            loader,
            trie: trie.clone(),
            trie_iter,
        })
    }

    pub fn thaw(&self) -> MutableState {
        let mut persistent_state = self.lock_read();
        MutableState(persistent_state.thaw())
    }

    fn lock_write(&self) -> RwLockWriteGuard<'_, trie::PersistentState> {
        self.0.write().expect("PersistentState lock poisoned")
    }

    fn lock_read(&self) -> RwLockReadGuard<'_, trie::PersistentState> {
        self.0.read().expect("PersistentState lock poisoned")
    }
}

struct PrefixIterator<'a, L> {
    loader: &'a L,
    trie: trie::MutableTrie,
    trie_iter: Option<trie::low_level::Iterator>,
}

impl<L> Drop for PrefixIterator<'_, L> {
    fn drop(&mut self) {
        if let Some(trie_iter) = self.trie_iter.as_ref() {
            self.trie.delete_iter(trie_iter);
        }
    }
}

impl<L> Iterator for PrefixIterator<'_, L>
where
    L: BlobStoreLoad,
{
    type Item = (Vec<u8>, Vec<u8>);

    fn next(&mut self) -> Option<Self::Item> {
        let trie_iter = self.trie_iter.as_mut()?;

        let mut loader = LoaderAdapter(self.loader);
        match self
            .trie
            .next(&mut loader, trie_iter, &mut trie::EmptyCounter)
        {
            Ok(Some(entry_id)) => {
                let value = self
                    .trie
                    .with_entry(entry_id, &mut loader, |value| value.to_vec())?;
                Some((trie_iter.get_key().to_vec(), value))
            }
            Ok(None) => None,
            Err(counter_err) => match counter_err {},
        }
    }
}

#[derive(Debug)]
pub struct MutableState(trie::MutableState);

impl MutableState {
    pub fn freeze(&mut self, loader: &impl BlobStoreLoad) -> PersistentState {
        PersistentState(RwLock::new(
            self.0
                .freeze(&mut LoaderAdapter(loader), &mut trie::EmptyCollector),
        ))
    }

    pub fn insert_value(
        &mut self,
        loader: &impl BlobStoreLoad,
        key: &[u8],
        value: Vec<u8>,
    ) -> BlockStateResult<()> {
        let mut loader = LoaderAdapter(loader);
        let mut trie = self.0.get_inner(&mut loader).lock();
        trie.insert(&mut loader, key, value).map_err(|err| {
            BlockStateError::Invariant(format!("Error deleting value from MutableState: {}", err))
        })?;
        Ok(())
    }

    pub fn delete_value(
        &mut self,
        loader: &impl BlobStoreLoad,
        key: &[u8],
    ) -> BlockStateResult<()> {
        let mut loader = LoaderAdapter(loader);
        let mut trie = self.0.get_inner(&mut loader).lock();
        trie.delete(&mut loader, key).map_err(|err| {
            BlockStateError::Invariant(format!("Error deleting value from MutableState: {}", err))
        })?;
        Ok(())
    }
}

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
        self.lock_write()
            .store_update_buf(&mut StorerAdapter(storer), &mut buffer)
            .expect("error writing PersistentState to blob store");
    }
}

struct LoaderAdapter<'a, L>(&'a L);

impl<'a, L: BlobStoreLoad> trie::BackingStoreLoad for LoaderAdapter<'a, L> {
    type R = Vec<u8>;

    fn load_raw(&mut self, location: trie::Reference) -> trie::LoadResult<Self::R> {
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
        self.lock_write().cache(&mut LoaderAdapter(loader));
        Ok(())
    }
}

impl Hashable for PersistentState {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        Ok(Hash::from(
            self.lock_write().hash(&mut LoaderAdapter(loader)).hash,
        ))
    }
}
