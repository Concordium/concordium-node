//! Definition of the [`Migrate`] trait that defines migration of block state values
//! from the blob store of the current protocol version blob store to the blob store of the next
//! protocol version (each protocol version has its own blob store).

use crate::block_state::blob_store::{BlobStoreLoad, BlobStoreStore, StoreSerialized};
use crate::block_state_interface::BlockStateResult;
use concordium_base::common::Serial;

/// Trait implemented by block state values to support migration when protocol version increments.
/// The migration must,
///
/// * recursively store all [blob references](super::blob_reference) into the new blob store
///   of we migrate to (the blob store of the new protocol version)
/// * apply any changes to the representation of the block state value (data model migration)
///
/// Since each protocol version has its own blob store, migration is always needed at
/// protocol update, even if the block state value has no data model changes.
pub trait Migrate {
    /// Migrate the value from the blob store it is currently stored in
    /// (`from_loader`), to the new blob store for the next protocol version (`to_storer`).
    /// Migration must:
    ///
    /// * recursively migrate all [blob references](super::blob_reference) the value is composed of
    ///   to the new blob store, including storing the referenced values in the new blob store
    /// * apply any changes to the value (data model migration)
    ///
    /// The function returns the new, migrated value, that represents the value on the new protocol
    /// version, and whose [blob references](super::blob_reference) points to the new blob store.
    ///
    /// The implementation must recursively make sure the same operations are applied to any
    /// block state components, the value may be composed of.
    ///
    /// # Arguments
    ///
    /// - `from_loader`: loader for the blob store that the value is currently stored in
    ///   (the blob store we migrate from)
    /// - `to_loader`: storer for the blob store that we migrate to
    fn migrate(
        &self,
        from_loader: &impl BlobStoreLoad,
        to_storer: &mut impl BlobStoreStore,
    ) -> BlockStateResult<Self>
    where
        Self: Sized;
}

impl<T: Serial + Clone> Migrate for StoreSerialized<T> {
    fn migrate(
        &self,
        _from_loader: &impl BlobStoreLoad,
        _to_storer: &mut impl BlobStoreStore,
    ) -> BlockStateResult<Self> {
        Ok(self.clone())
    }
}
