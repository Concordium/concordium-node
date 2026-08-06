use crate::failure::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_store::{
    BlobStoreLoad, BlobStoreLocation, BlobStoreStore, Loadable, Storable,
};
use crate::persistent::cacheable::Cacheable;
use crate::persistent::chain_parameters::p11::PersistentChainParametersP11;
use crate::persistent::hash::Hashable;
use concordium_base::common::Buffer;
use concordium_base::contracts_common::Duration;
use concordium_base::hashes::Hash;
use plt_scheduler_types::types::protocol_version::ProtocolVersion;
use std::any;
use std::io::Read;

pub mod p11;

/// Persistent node-owned chain parameters managed by Rust.
#[derive(Debug, Clone)]
pub enum PersistentChainParameters {
    /// P11 external chain parameters.
    P11(PersistentChainParametersP11),
}

impl PersistentChainParameters {
    /// Construct P11 persistent chain parameters with an initial maximum lock duration.
    pub fn p11_new_external_chain_parameters(max_lock_duration: Duration) -> Self {
        Self::P11(PersistentChainParametersP11 { max_lock_duration })
    }

    /// Apply a max-lock-duration update to the Rust-managed chain parameters.
    pub fn apply_max_lock_duration_update(&mut self, max_lock_duration: Duration) {
        match self {
            Self::P11(params) => params.max_lock_duration = max_lock_duration,
        }
    }

    /// Load persistent chain parameters from the blob store.
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
                any::type_name::<PersistentChainParameters>()
            )));
        };
        Ok(value)
    }

    /// Load persistent chain parameters from bytes for the given protocol version.
    fn load_from_buffer(
        buffer: impl Read,
        loader: &impl BlobStoreLoad,
        protocol_version: ProtocolVersion,
    ) -> BlockStateResult<Self> {
        match protocol_version {
            ProtocolVersion::P11 => Ok(Self::P11(Loadable::load_from_buffer(buffer, loader)?)),
            ProtocolVersion::P9 | ProtocolVersion::P10 => {
                panic!("No Rust-managed external chain parameters before P11")
            }
        }
    }
}

impl Storable for PersistentChainParameters {
    fn store_to_buffer(&self, buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        match self {
            PersistentChainParameters::P11(params) => params.store_to_buffer(buffer, storer),
        }
    }
}

impl Cacheable for PersistentChainParameters {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        match self {
            PersistentChainParameters::P11(params) => params.cache_reference_values(loader),
        }
    }
}

impl Hashable for PersistentChainParameters {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        match self {
            PersistentChainParameters::P11(params) => params.hash(loader),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::persistent::blob_store;
    use crate::persistent::blob_store::test_stub::BlobStoreStub;

    #[test]
    fn store_load_roundtrip() {
        let mut store = BlobStoreStub::default();
        let params =
            PersistentChainParameters::p11_new_external_chain_parameters(Duration::from_millis(42));
        let location = blob_store::store_to_store(&mut store, &params);
        let loaded =
            PersistentChainParameters::load_from_store(&store, location, ProtocolVersion::P11)
                .expect("external chain parameters should load");

        match loaded {
            PersistentChainParameters::P11(params) => {
                assert_eq!(params.max_lock_duration, Duration::from_millis(42))
            }
        }
    }
}
