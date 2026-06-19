//! Helpers for migrating a block state between protocol versions in the scheduler integration
//! tests.

use plt_block_state::entity::EntityContext;
use plt_block_state::entity::block_state::p9::BlockStateP9;
use plt_block_state::entity::block_state::p10::BlockStateP10;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::entity_test_stub::{self, StubbedEntityContext};
use plt_block_state::persistent::blob_store;
use plt_block_state::persistent::block_state::PersistentBlockState;
use plt_scheduler_types::types::protocol_version::ProtocolVersion;

// todo ar inspect

/// Migrate a P9 block state to P10. es.
pub fn migrate_p9_to_p10(
    context: &mut StubbedEntityContext,
    block_state: BlockStateP9,
) -> (StubbedEntityContext, BlockStateP10) {
    // Flush the source block state so all referenced blobs are present in the source store.
    blob_store::store_to_store(&mut context.store, &block_state.persistent);

    let mut migrated_context = entity_test_stub::new_stubbed_context();
    migrated_context.external = context.external.clone();

    let migrated_persistent = PersistentBlockState::P9(block_state.persistent)
        .migrate(
            &context.store,
            &mut migrated_context.store,
            ProtocolVersion::P10,
        )
        .expect("migrate P9 to P10");
    let blob_ref = blob_store::store_to_store(&mut migrated_context.store, &migrated_persistent);
    let migrated_block_state = BlockStateP10 {
        persistent: blob_store::load_from_store(&migrated_context.store, blob_ref).unwrap(),
    };

    (migrated_context, migrated_block_state)
}

/// Migrate a P10 block state to P11.
pub fn migrate_p10_to_p11(
    context: &mut StubbedEntityContext,
    block_state: BlockStateP10,
) -> (StubbedEntityContext, BlockStateP11) {
    // Flush the source block state so all referenced blobs are present in the source store.
    blob_store::store_to_store(&mut context.store, &block_state.persistent);

    let mut migrated_context = entity_test_stub::new_stubbed_context();
    migrated_context.external = context.external.clone();

    let migrated_persistent = PersistentBlockState::P10(block_state.persistent)
        .migrate(
            &context.store,
            &mut migrated_context.store,
            ProtocolVersion::P11,
        )
        .expect("migrate P10 to P11");
    let blob_ref = blob_store::store_to_store(&mut migrated_context.store, &migrated_persistent);
    let migrated_block_state = BlockStateP11 {
        persistent: blob_store::load_from_store(&migrated_context.store, blob_ref).unwrap(),
    };

    (migrated_context, migrated_block_state)
}
