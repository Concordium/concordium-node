//! Definition of the [`Migrate`] trait that defines migration of block state values
//! from the blob store of the current protocol version blob store to the blob store of the next
//! protocol version.

use crate::block_state::blob_store::{BlobStoreLoad, Storable, StoreSerialized};
use crate::block_state_interface::BlockStateResult;
use concordium_base::common::{Put, Serial};
use concordium_base::hashes::Hash;
use sha2::Digest;

/// Trait implemented by block state values to support migration when protocol version increments.
/// Migration must
///
/// 1. copy the value from the blob store of the current protocol version to the blob store
///   of the next protocol version
/// 2. apply any changes to the representation of the block state value (data model migration)
///
/// Step 1. must always be performed, even if there are no changes to the representation of the
/// value in the blob store.
pub trait Migrate {
    /// Migrate the value from the blob store it is currently stored in
    /// (`from_loader`) to the blob store for the next protocol version (`to_storer`).
    /// Returns a copy of the value, that is stored in the destination blob store.
    /// The migration must
    ///
    /// 1. load any needed data that is not already in memory via `from_loader`
    /// 2. make a copy of the value, while applying changes to the representation of the value
    ///   if needed by the protocol update
    /// 3. store the new value to the destination blob store
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
        to_storer: &mut impl BlobStoreLoad,
    ) -> BlockStateResult<Self> where Self:Sized;
}

impl<T: Serial + Clone> Migrate for StoreSerialized<T> {
    fn migrate(
        &self,
        _from_loader: &impl BlobStoreLoad,
        _to_storer: &mut impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        Ok(self.clone())
    }
}
