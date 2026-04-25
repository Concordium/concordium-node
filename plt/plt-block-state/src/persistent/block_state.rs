use crate::block_state::blob_store::{
    BlobStoreLoad, BlobStoreLocation, BlobStoreStore, Loadable, Storable,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use crate::persistent::block_state::p9::PersistentBlockStateP9;
use crate::persistent::block_state::p10::PersistentBlockStateP10;
use crate::persistent::block_state::p11::PersistentBlockStateP11;
use concordium_base::common::Buffer;
use concordium_base::hashes::Hash;
use plt_scheduler_types::types::protocol_version::ProtocolVersion;
use std::any;
use std::io::Read;

pub mod p10;
pub mod p11;
pub mod p9;

/// Immutable block state. The block state is immutable in the sense,
/// that the state it represents never changes during the lifetime of values of type [`PersistentBlockState`].
/// In order to perform mutating operations on the block state, a new [`PersistentBlockState`]
/// must be created.
///
/// The internal representation in [`PersistentBlockState`] may change during the lifetime via interior mutability.
/// This happens if state are cached, stored or hashes are lazily calculated.
#[derive(Debug, Clone)]
pub enum PersistentBlockState {
    P9(PersistentBlockStateP9),
    P10(PersistentBlockStateP10),
    P11(PersistentBlockStateP11),
}

impl PersistentBlockState {
    /// Construct an empty block state.
    pub fn empty(protocol_version: ProtocolVersion) -> Self {
        match protocol_version {
            ProtocolVersion::P9 => Self::P9(Default::default()),
            ProtocolVersion::P10 => Self::P10(Default::default()),
            ProtocolVersion::P11 => Self::P11(Default::default()),
        }
    }

    /// See [`blob_store::load_from_store`]. This function only differs by taking
    /// protocol version as argument.
    pub fn load_from_store(
        loader: &impl BlobStoreLoad,
        location: BlobStoreLocation,
        protocol_version: ProtocolVersion,
    ) -> BlockStateResult<Self> {
        let bytes = loader.load_raw(location);
        let mut bytes_slice = bytes.as_slice();
        let value = Self::load_from_buffer(&mut bytes_slice, loader, protocol_version)?;
        if !bytes_slice.is_empty() {
            return Err(BlockStateFailure::BlobStoreDecode(format!(
                "Bytes remaining after loading value of type {} from blob store",
                any::type_name::<PersistentBlockState>()
            )));
        };
        Ok(value)
    }

    /// See [`Loadable::load_from_buffer`]. This function only differs by taking
    /// protocol version as argument.
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
        protocol_version: ProtocolVersion,
    ) -> BlockStateResult<Self> {
        Ok(match protocol_version {
            ProtocolVersion::P9 => Self::P9(Loadable::load_from_buffer(buffer, loader)?),
            ProtocolVersion::P10 => Self::P10(Loadable::load_from_buffer(buffer, loader)?),
            ProtocolVersion::P11 => Self::P11(Loadable::load_from_buffer(buffer, loader)?),
        })
    }

    /// Migrate the PLT block state from one blob store to another.
    ///
    /// # Arguments
    ///
    /// - `from_loader` Blob store loader for the blob store we are migrating from.
    /// - `to_storer` Blob store storer for the blob store we are migrating to.
    /// - `to_protocol_version` Protocol version for the block state to migrate to.
    pub fn migrate(
        &self,
        _from_loader: impl BlobStoreLoad,
        _to_storer: impl BlobStoreStore,
        _to_protocol_version: ProtocolVersion,
    ) -> Self {
        // todo ar
        todo!()
    }
}

impl Storable for PersistentBlockState {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        match self {
            PersistentBlockState::P9(bs) => bs.store_to_buffer(buffer, storer),
            PersistentBlockState::P10(bs) => bs.store_to_buffer(buffer, storer),
            PersistentBlockState::P11(bs) => bs.store_to_buffer(buffer, storer),
        }
    }
}

impl Cacheable for PersistentBlockState {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        match self {
            PersistentBlockState::P9(bs) => bs.cache_reference_values(loader),
            PersistentBlockState::P10(bs) => bs.cache_reference_values(loader),
            PersistentBlockState::P11(bs) => bs.cache_reference_values(loader),
        }
    }
}

impl Hashable for PersistentBlockState {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        match self {
            PersistentBlockState::P9(bs) => bs.hash(loader),
            PersistentBlockState::P10(bs) => bs.hash(loader),
            PersistentBlockState::P11(bs) => bs.hash(loader),
        }
    }
}
