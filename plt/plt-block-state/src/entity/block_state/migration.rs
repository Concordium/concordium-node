use crate::entity::block_state::p9::BlockStateP9;
use crate::entity::block_state::p10::BlockStateP10;
use crate::failure::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_store::{BlobStoreLoad, BlobStoreStore};
use crate::persistent::block_state::PersistentBlockState;
use plt_scheduler_types::types::protocol_version::ProtocolVersion;

pub mod p10_to_p11;
pub mod p9_to_p10;

/// Migrate the PLT block state from protocol version to another. The persisten block state
/// is first "lifted" into an entity block state and then migrated.
///
/// # Arguments
///
/// - `from_store` Blob store loader for the blob store we are migrating from.
/// - `to_store` Blob store loader and storer for the blob store we are migrating to.
/// - `to_protocol_version` Protocol version for the block state to migrate to.
pub fn migrate(
    block_state: PersistentBlockState,
    from_store: &impl BlobStoreLoad,
    to_store: &mut (impl BlobStoreStore + BlobStoreLoad),
    to_protocol_version: ProtocolVersion,
) -> BlockStateResult<PersistentBlockState> {
    match block_state {
        PersistentBlockState::P9(persistent_block_state) => {
            let block_state = BlockStateP9 {
                persistent: persistent_block_state,
            };
            let new_block_state =
                p9_to_p10::migrate_from_p9_to_p10(block_state, from_store, to_store)?;
            assert_eq!(to_protocol_version, ProtocolVersion::P10);
            Ok(PersistentBlockState::P10(new_block_state.persistent))
        }
        PersistentBlockState::P10(persistent_block_state) => {
            let block_state = BlockStateP10 {
                persistent: persistent_block_state,
            };
            let new_block_state =
                p10_to_p11::migrate_from_p10_to_p11(block_state, from_store, to_store)?;
            assert_eq!(to_protocol_version, ProtocolVersion::P11);
            Ok(PersistentBlockState::P11(new_block_state.persistent))
        }
        PersistentBlockState::P11(_) => Err(BlockStateFailure::Invariant(
            "migration of P11 block state not implemented".to_string(),
        )),
    }
}

pub mod test_utils {
    use super::*;
    use crate::entity::block_state::p9::BlockStateP9;
    use crate::entity::block_state::p10::BlockStateP10;
    use crate::entity::block_state::p11::BlockStateP11;
    use crate::entity::entity_test_stub;
    use crate::entity::entity_test_stub::StubbedEntityContext;
    use crate::persistent::blob_store;
    use crate::persistent::block_state::PersistentBlockState;

    /// Migrate a P9 block state store in the test stub to P10.
    pub fn migrate_p9_to_p10(
        context: &mut StubbedEntityContext,
        block_state: BlockStateP9,
    ) -> (StubbedEntityContext, BlockStateP10) {
        // Flush the source block state so all referenced blobs are present in the source store.
        blob_store::store_to_store(&mut context.store, &block_state.persistent);

        let mut migrated_context = entity_test_stub::new_stubbed_context();
        migrated_context.external = context.external.clone();

        // Migrate the block state
        let migrated_persistent = migrate(
            PersistentBlockState::P9(block_state.persistent),
            &context.store,
            &mut migrated_context.store,
            ProtocolVersion::P10,
        )
        .expect("migrate P9 to P10");

        // Store and load the migrated block state
        let blob_ref =
            blob_store::store_to_store(&mut migrated_context.store, &migrated_persistent);
        let migrated_block_state = BlockStateP10 {
            persistent: blob_store::load_from_store(&migrated_context.store, blob_ref).unwrap(),
        };

        (migrated_context, migrated_block_state)
    }

    /// Migrate a P10 block state store in the test stub to P11.
    pub fn migrate_p10_to_p11(
        context: &mut StubbedEntityContext,
        block_state: BlockStateP10,
    ) -> (StubbedEntityContext, BlockStateP11) {
        // Flush the source block state so all referenced blobs are present in the source store.
        blob_store::store_to_store(&mut context.store, &block_state.persistent);

        let mut migrated_context = entity_test_stub::new_stubbed_context();
        migrated_context.external = context.external.clone();

        // Migrate the block state
        let migrated_persistent = migrate(
            PersistentBlockState::P10(block_state.persistent),
            &context.store,
            &mut migrated_context.store,
            ProtocolVersion::P11,
        )
        .expect("migrate P9 to P10");

        // Store and load the migrated block state
        let blob_ref =
            blob_store::store_to_store(&mut migrated_context.store, &migrated_persistent);
        let migrated_block_state = BlockStateP11 {
            persistent: blob_store::load_from_store(&migrated_context.store, blob_ref).unwrap(),
        };

        (migrated_context, migrated_block_state)
    }
}
