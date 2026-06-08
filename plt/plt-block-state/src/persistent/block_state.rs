use crate::failure::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_store::{
    BlobStoreLoad, BlobStoreLocation, BlobStoreStore, Loadable, Storable,
};
use crate::persistent::block_state::p9::PersistentBlockStateP9;
use crate::persistent::block_state::p10::PersistentBlockStateP10;
use crate::persistent::block_state::p11::PersistentBlockStateP11;
use crate::persistent::cacheable::Cacheable;
use crate::persistent::hash::Hashable;
use crate::persistent::migration::Migrate;
use concordium_base::common::Buffer;
use concordium_base::hashes::Hash;
use plt_scheduler_types::types::protocol_version::ProtocolVersion;
use std::any;
use std::io::Read;

pub mod p10;
pub mod p11;
pub mod p9;

/// Persistent block that that may represent any protocol version know to the Rust scheduler.
#[derive(Debug, Clone)]
pub enum PersistentBlockState {
    P9(PersistentBlockStateP9),
    P10(PersistentBlockStateP10),
    P11(PersistentBlockStateP11),
}

impl PersistentBlockState {
    /// Construct an empty block state for the given protocol version.
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
        buffer: impl Read,
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
        from_loader: impl BlobStoreLoad,
        mut to_storer: impl BlobStoreStore,
        to_protocol_version: ProtocolVersion,
    ) -> BlockStateResult<Self> {
        match self {
            PersistentBlockState::P9(block_state) => {
                let new_block_state = block_state.migrate(&from_loader, &mut to_storer)?;
                assert_eq!(to_protocol_version, ProtocolVersion::P10);
                Ok(Self::P10(new_block_state))
            }
            PersistentBlockState::P10(block_state) => {
                let new_block_state = block_state.migrate(&from_loader, &mut to_storer)?;
                assert_eq!(to_protocol_version, ProtocolVersion::P11);
                Ok(Self::P11(new_block_state))
            }
            PersistentBlockState::P11(_) => Err(BlockStateFailure::Invariant(
                "migration of P11 block state not implemented".to_string(),
            )),
        }
        // todo ar trigger store here at top level?
    }
}

impl Storable for PersistentBlockState {
    fn store_to_buffer(&self, buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
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
