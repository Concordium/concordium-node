use crate::entity::block_state::p9::BlockStateP9;
use crate::entity::block_state::p10::BlockStateP10;
use crate::failure::BlockStateResult;
use crate::persistent::blob_store::{BlobStoreLoad, BlobStoreMovable, BlobStoreStore};

/// Migrate the P9 block state to P10.
pub fn migrate_from_p9_to_p10(
    block_state_p9: BlockStateP9,
    from_store: &impl BlobStoreLoad,
    to_store: &mut impl BlobStoreStore,
) -> BlockStateResult<BlockStateP10> {
    // There are no changes to data, so just move to new blob store.
    let new_persistent = block_state_p9
        .persistent
        .move_blob_store(from_store, to_store)?;

    Ok(BlockStateP10 {
        persistent: new_persistent,
    })
}

#[cfg(test)]
mod test {
    use crate::entity::block_state::migration;
    use crate::entity::block_state::p9::BlockStateP9;
    use crate::entity::entity_test_stub;
    use crate::persistent::protocol_level_tokens::p9::TokenConfiguration;
    use concordium_base::protocol_level_tokens::TokenModuleRef;
    use plt_scheduler_types::types::tokens::RawTokenAmount;

    /// Migrate block state from P9 to P10.
    #[test]
    fn test_migrate_p9_to_p10() {
        let mut context = entity_test_stub::new_stubbed_context();
        let mut block_state = BlockStateP9::default();

        // Create tokens
        let configuration1 = TokenConfiguration {
            token_id: "token1".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 2,
        };
        let token_index1 = block_state
            .create_token(&context, configuration1.clone())
            .unwrap();
        let mut token1 = block_state.token_by_index(&context, token_index1).unwrap();
        token1
            .token_p9_base
            .set_token_circulating_supply(RawTokenAmount(100));
        token1
            .token_p9_base
            .set_deny_list_enabled(&context)
            .unwrap();
        token1
            .token_p9_base
            .set_token_name(&context, "token1name")
            .unwrap();
        block_state.update_token(&context, token1).unwrap();
        let configuration2 = TokenConfiguration {
            token_id: "token2".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 4,
        };
        let _token_index2 = block_state.create_token(&context, configuration2.clone());

        // Migrate the block state
        let (migrated_context, migrated_block_state) =
            migration::test_utils::migrate_p9_to_p10(&mut context, block_state);

        // Assert on migrated block state
        assert_eq!(migrated_block_state.plt_list(&migrated_context).len(), 2);
        let token1 = migrated_block_state
            .token_by_id(&migrated_context, &"token1".parse().unwrap())
            .unwrap()
            .unwrap();
        assert_eq!(
            token1.token_p9_base.token_circulating_supply(),
            RawTokenAmount(100)
        );
        assert_eq!(
            token1
                .token_p9_base
                .token_configuration(&migrated_context)
                .unwrap(),
            configuration1
        );
        assert!(token1.token_p9_base.has_deny_list(&migrated_context));
        assert_eq!(
            token1
                .token_p9_base
                .get_token_name(&migrated_context)
                .unwrap(),
            "token1name"
        );
        let token2 = migrated_block_state
            .token_by_id(&migrated_context, &"token2".parse().unwrap())
            .unwrap()
            .unwrap();
        assert_eq!(
            token2.token_p9_base.token_circulating_supply(),
            RawTokenAmount(0)
        );
        assert_eq!(
            token2
                .token_p9_base
                .token_configuration(&migrated_context)
                .unwrap(),
            configuration2
        );
    }
}
