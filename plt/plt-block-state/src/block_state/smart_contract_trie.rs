//! Adapter for the trie in the `concordium-smart-contract-engine` crate. There is an
//! impedance mismatch between the Rust block state and the smart contract trie, on how
//! mutability (thawing/freezing) is handled, at which level interior mutability (via locks) is implemented,
//! and the specific definitions of the blob store traits. Hence, this adapter is needed to use
//! the smart contract trie in the Rust block state.

use crate::block_state::blob_store::{
    BlobStoreLoad, BlobStoreLocation, BlobStoreStore, Loadable, Storable,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use crate::block_state_interface::{BlockStateError, BlockStateResult};
use concordium_base::common::Buffer;
use concordium_base::hashes::Hash;
use concordium_smart_contract_engine::v1::trie;
use std::io::Read;
use std::sync::{Mutex, MutexGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};

/// Immutable (persistent) trie. The internal structure may be changed via interior mutability,
/// but the entries in the trie never changes. This is the frozen/persistent dual to [`MutableState`].
#[derive(Debug)]
pub struct PersistentState(RwLock<trie::PersistentState>);

impl PersistentState {
    /// Create empty trie.
    pub fn empty() -> Self {
        Self(RwLock::new(trie::PersistentState::Empty))
    }

    /// Lookup the value in the trie for the given key and return the value.
    /// Returns `None` if there is no entry for the given key.
    pub fn lookup_value(&self, loader: &impl BlobStoreLoad, key: &[u8]) -> Option<Vec<u8>> {
        let mut loader = LoaderAdapter(loader);
        let persistent_state = self.lock_read();
        persistent_state.lookup(&mut loader, key)
    }

    /// Iterate entries whose keys start with the given prefix. Returns an iterator over
    /// key-value pairs.
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

    /// Thaw the trie to make it [mutable](MutableState).
    pub fn thaw(&self) -> MutableState {
        let persistent_state = self.lock_read();
        MutableState(Mutex::new(persistent_state.thaw()))
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

/// Mutable trie. This is the thawed/mutable dual to [`PersistentState`].
#[derive(Debug)]
pub struct MutableState(Mutex<trie::MutableState>);

impl MutableState {
    /// Freeze the trie to make it [persistent](PersistentState).
    pub fn freeze(&mut self, loader: &impl BlobStoreLoad) -> PersistentState {
        PersistentState(RwLock::new(
            self.lock()
                .freeze(&mut LoaderAdapter(loader), &mut trie::EmptyCollector),
        ))
    }

    /// Lookup the value in the trie for the given key and return the value.
    /// Returns `None` if there is no entry for the given key.
    pub fn lookup_value(&self, loader: &impl BlobStoreLoad, key: &[u8]) -> Option<Vec<u8>> {
        let mut loader_adapter = LoaderAdapter(loader);
        let mut loader = LoaderAdapter(loader);
        let mut mutable_state = self.lock();
        let mut trie = mutable_state.get_inner(&mut loader_adapter).lock();
        let entry_id = trie.get_entry(&mut loader, key)?;
        trie.with_entry(entry_id, &mut loader, |value| value.to_vec())
    }

    /// Iterate entries whose keys start with the given prefix. Returns an iterator over
    /// key-value pairs.
    pub fn iter_prefix<'a, L: BlobStoreLoad>(
        &self,
        loader: &'a L,
        prefix: &[u8],
    ) -> BlockStateResult<impl Iterator<Item = (Vec<u8>, Vec<u8>)> + use<'a, L>> {
        let mut loader_adapter = LoaderAdapter(loader);
        let mut mutable_state = self.lock();
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

    /// Insert or update the value for the given key. If no entry exists in the trie
    /// for the given key, the value is inserted. If an entry already exists
    /// for the given key, the value is updated.
    pub fn insert_value(
        &mut self,
        loader: &impl BlobStoreLoad,
        key: &[u8],
        value: Vec<u8>,
    ) -> BlockStateResult<()> {
        let mut loader = LoaderAdapter(loader);
        let mut trie = self.get_mut().get_inner(&mut loader).lock();
        trie.insert(&mut loader, key, value).map_err(|err| {
            BlockStateError::Invariant(format!("Error deleting value from MutableState: {}", err))
        })?;
        Ok(())
    }

    /// Delete the value for the given key. This is a no-op, if no entry exists in the trie
    /// for the given key.
    pub fn delete_value(
        &mut self,
        loader: &impl BlobStoreLoad,
        key: &[u8],
    ) -> BlockStateResult<()> {
        let mut loader = LoaderAdapter(loader);
        let mut trie = self.get_mut().get_inner(&mut loader).lock();
        trie.delete(&mut loader, key).map_err(|err| {
            BlockStateError::Invariant(format!("Error deleting value from MutableState: {}", err))
        })?;
        Ok(())
    }

    fn lock(&self) -> MutexGuard<'_, trie::MutableState> {
        self.0.lock().expect("MutableState lock poisoned")
    }

    fn get_mut(&mut self) -> &mut trie::MutableState {
        self.0.get_mut().expect("MutableState lock poisoned")
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

#[cfg(test)]
mod test {
    use crate::block_state::blob_store;
    use crate::block_state::blob_store::test_stub::{BlobStoreStub, UnreachableBlobStore};
    use crate::block_state::cacheable::Cacheable;
    use crate::block_state::smart_contract_trie::PersistentState;

    #[test]
    fn test_insert_delete_and_lookup() {
        let state = PersistentState::empty();

        // Insert entries
        let mut mutable_state = state.thaw();
        mutable_state
            .insert_value(&UnreachableBlobStore, &[0, 1], vec![1, 1])
            .unwrap();
        mutable_state
            .insert_value(&UnreachableBlobStore, &[0, 2], vec![2, 2])
            .unwrap();

        // Lookup values in mutable state
        assert_eq!(
            mutable_state.lookup_value(&UnreachableBlobStore, &[0, 1]),
            Some(vec![1, 1])
        );
        assert_eq!(
            mutable_state.lookup_value(&UnreachableBlobStore, &[0, 2]),
            Some(vec![2, 2])
        );
        assert_eq!(
            mutable_state.lookup_value(&UnreachableBlobStore, &[0, 3]),
            None
        );

        // Freeze state
        let state = mutable_state.freeze(&UnreachableBlobStore);

        // Lookup values in persistent state
        assert_eq!(
            state.lookup_value(&UnreachableBlobStore, &[0, 1]),
            Some(vec![1, 1])
        );
        assert_eq!(
            state.lookup_value(&UnreachableBlobStore, &[0, 2]),
            Some(vec![2, 2])
        );
        assert_eq!(state.lookup_value(&UnreachableBlobStore, &[0, 3]), None);

        // Update and delete entries
        let mut mutable_state = state.thaw();
        mutable_state
            .insert_value(&UnreachableBlobStore, &[0, 1], vec![4, 4])
            .unwrap();
        mutable_state
            .delete_value(&UnreachableBlobStore, &[0, 2])
            .unwrap();


        // Lookup values in mutable state
        assert_eq!(
            mutable_state.lookup_value(&UnreachableBlobStore, &[0, 1]),
            Some(vec![4, 4])
        );
        assert_eq!(
            mutable_state.lookup_value(&UnreachableBlobStore, &[0, 2]),
            None
        );

        // Freeze state
        let state = mutable_state.freeze(&UnreachableBlobStore);

        // Lookup values in persistent state
        assert_eq!(
            state.lookup_value(&UnreachableBlobStore, &[0, 1]),
            Some(vec![4, 4])
        );
        assert_eq!(state.lookup_value(&UnreachableBlobStore, &[0, 2]), None);
    }

    #[test]
    fn test_iter_prefix() {
        let state = PersistentState::empty();

        // Insert entries
        let mut mutable_state = state.thaw();
        mutable_state
            .insert_value(&UnreachableBlobStore, &[0, 1], vec![1, 1])
            .unwrap();
        mutable_state
            .insert_value(&UnreachableBlobStore, &[0, 2], vec![2, 2])
            .unwrap();
        mutable_state
            .insert_value(&UnreachableBlobStore, &[1, 1], vec![3, 3])
            .unwrap();
        mutable_state
            .insert_value(&UnreachableBlobStore, &[1, 2], vec![4, 4])
            .unwrap();
        mutable_state
            .insert_value(&UnreachableBlobStore, &[2, 1], vec![5, 5])
            .unwrap();


        // Iterate values in mutable state
        let values: Vec<_> = mutable_state
            .iter_prefix(&UnreachableBlobStore, &[0, 2])
            .unwrap()
            .collect();
        assert_eq!(values, vec![(vec![0, 2], vec![2, 2])]);
        let values: Vec<_> = mutable_state
            .iter_prefix(&UnreachableBlobStore, &[1])
            .unwrap()
            .collect();
        assert_eq!(
            values,
            vec![(vec![1, 1], vec![3, 3]), (vec![1, 2], vec![4, 4])]
        );
        let values: Vec<_> = mutable_state
            .iter_prefix(&UnreachableBlobStore, &[3])
            .unwrap()
            .collect();
        assert_eq!(values, vec![]);

        // Freeze state
        let state = mutable_state.freeze(&UnreachableBlobStore);

        // Iterate values in persistent state
        let values: Vec<_> = state
            .iter_prefix(&UnreachableBlobStore, &[0, 2])
            .unwrap()
            .collect();
        assert_eq!(values, vec![(vec![0, 2], vec![2, 2])]);
        let values: Vec<_> = state
            .iter_prefix(&UnreachableBlobStore, &[1])
            .unwrap()
            .collect();
        assert_eq!(
            values,
            vec![(vec![1, 1], vec![3, 3]), (vec![1, 2], vec![4, 4])]
        );
        let values: Vec<_> = state
            .iter_prefix(&UnreachableBlobStore, &[3])
            .unwrap()
            .collect();
        assert_eq!(values, vec![]);
    }

    #[test]
    fn test_store_load_and_cache() {
        let mut store = BlobStoreStub::default();
        let state = PersistentState::empty();

        // Insert entries
        let mut mutable_state = state.thaw();
        mutable_state
            .insert_value(&store, &[0, 1], vec![1, 1])
            .unwrap();
        mutable_state
            .insert_value(&store, &[0, 2], vec![2, 2])
            .unwrap();
        let state = mutable_state.freeze(&store);

        // Store trie
        let blob_ref = blob_store::store_to_store(&mut store, state);

        // Load trie
        let state: PersistentState = blob_store::load_from_store(&store, blob_ref).unwrap();

        // Lookup values
        assert_eq!(state.lookup_value(&store, &[0, 1]), Some(vec![1, 1]));
        assert_eq!(state.lookup_value(&store, &[0, 2]), Some(vec![2, 2]));

        // Cache trie
        state.cache_reference_values(&store).unwrap();

        // Lookup values using "unreachable" blob store since entries
        // should be in memory now.
        assert_eq!(
            state.lookup_value(&UnreachableBlobStore, &[0, 1]),
            Some(vec![1, 1])
        );
        assert_eq!(
            state.lookup_value(&UnreachableBlobStore, &[0, 2]),
            Some(vec![2, 2])
        );
    }
}
