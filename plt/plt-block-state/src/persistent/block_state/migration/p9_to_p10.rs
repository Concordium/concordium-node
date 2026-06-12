#[cfg(test)]
mod test {
    use crate::entity::block_state::p9::BlockStateP9;
    use crate::entity::block_state::p10::BlockStateP10;
    use crate::entity::entity_test_stub;
    use crate::persistent::blob_store;
    use crate::persistent::block_state::PersistentBlockState;
    use crate::persistent::protocol_level_tokens::p9::TokenConfiguration;
    use concordium_base::protocol_level_tokens::TokenModuleRef;
    use plt_scheduler_types::types::protocol_version::ProtocolVersion;
    use plt_scheduler_types::types::tokens::RawTokenAmount;

    /// Move block state from one blob store to another.
    #[test]
    fn test_migrate_p9_to_p10() {
        let mut context = entity_test_stub::new_no_external_context();
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
            .mutable_key_value_state
            .insert_value(&context.loader, &[0, 1], vec![0, 0])
            .unwrap();
        token1
            .token_p9_base
            .mutable_key_value_state
            .insert_value(&context.loader, &[0, 2], vec![1, 1])
            .unwrap();
        block_state.update_token(&context, token1).unwrap();
        let configuration2 = TokenConfiguration {
            token_id: "token2".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 4,
        };
        let _token_index2 = block_state.create_token(&context, configuration2.clone());
        blob_store::store_to_store(&mut context.loader, &block_state.persistent);

        // Migrate the block state
        let mut migrated_context = entity_test_stub::new_no_external_context();
        let migrated_persistent_block_state = PersistentBlockState::P9(block_state.persistent)
            .migrate(
                &context.loader,
                &mut migrated_context.loader,
                ProtocolVersion::P10,
            )
            .unwrap();
        let blob_ref = blob_store::store_to_store(
            &mut migrated_context.loader,
            &migrated_persistent_block_state,
        );
        let migrated_block_state = BlockStateP10 {
            persistent: blob_store::load_from_store(&migrated_context.loader, blob_ref).unwrap(),
        };

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
        let value = token1
            .token_p9_base
            .mutable_key_value_state
            .lookup_value(&migrated_context.loader, &[0, 1]);
        assert_eq!(value, Some(vec![0, 0]));
        let value = token1
            .token_p9_base
            .mutable_key_value_state
            .lookup_value(&migrated_context.loader, &[0, 2]);
        assert_eq!(value, Some(vec![1, 1]));
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
