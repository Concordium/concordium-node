use crate::failure::BlockStateResult;
use crate::persistent::blob_store::{BlobStoreLoad, BlobStoreStore};
use crate::persistent::block_state::p9::PersistentBlockStateP9;
use crate::persistent::migration::Migrate;

/// P10 block state.
pub type PersistentBlockStateP10 = PersistentBlockStateP9;

impl Migrate<PersistentBlockStateP10> for PersistentBlockStateP9 {
    fn migrate(
        &self,
        from_loader: &impl BlobStoreLoad,
        to_storer: &mut impl BlobStoreStore,
    ) -> BlockStateResult<PersistentBlockStateP10>
    where
        Self: Sized,
    {
        let new_tokens = self.tokens.migrate(from_loader, to_storer)?;

        Ok(PersistentBlockStateP10 { tokens: new_tokens })
    }
}

#[cfg(test)]
mod test {
    use crate::entity::block_state::p9::BlockStateP9;
    use crate::entity::block_state::p10::BlockStateP10;
    use crate::entity::entity_test_stub;
    use crate::persistent::blob_store;
    use crate::persistent::block_state::p10::PersistentBlockStateP10;
    use crate::persistent::migration::Migrate;
    use crate::persistent::protocol_level_tokens::p9::TokenConfiguration;
    use concordium_base::protocol_level_tokens::TokenModuleRef;
    use plt_scheduler_types::types::tokens::RawTokenAmount;

    /// Migrate PLTs from P9 blob store to P10 blob store.
    #[test]
    fn test_migrate_tokens_p9_p10() {
        let mut context_p9 = entity_test_stub::new_no_external_context();
        let mut block_state_p9 = BlockStateP9::default();

        // Create tokens
        let configuration1 = TokenConfiguration {
            token_id: "token1".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 2,
        };
        let token_index1 = block_state_p9
            .create_token(&context_p9, configuration1.clone())
            .unwrap();
        let mut token1 = block_state_p9
            .token_by_index(&context_p9, token_index1)
            .unwrap();
        token1
            .token_p9_base
            .set_token_circulating_supply(RawTokenAmount(100));
        token1
            .token_p9_base
            .mutable_key_value_state
            .insert_value(&context_p9.loader, &[0, 1], vec![0, 0])
            .unwrap();
        token1
            .token_p9_base
            .mutable_key_value_state
            .insert_value(&context_p9.loader, &[0, 2], vec![1, 1])
            .unwrap();
        block_state_p9.update_token(&context_p9, token1).unwrap();
        let configuration2 = TokenConfiguration {
            token_id: "token2".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 4,
        };
        let _token_index2 = block_state_p9.create_token(&context_p9, configuration2.clone());
        let blob_ref =
            blob_store::store_to_store(&mut context_p9.loader, &block_state_p9.persistent);

        // Migrate block state
        let mut context_p10 = entity_test_stub::new_no_external_context();
        let persistent_block_state_p10: PersistentBlockStateP10 = block_state_p9
            .persistent
            .migrate(&context_p9.loader, &mut context_p10.loader)
            .unwrap();
        let blob_ref =
            blob_store::store_to_store(&mut context_p10.loader, &persistent_block_state_p10);
        let block_state_p10 = BlockStateP10 {
            persistent: blob_store::load_from_store(&context_p10.loader, blob_ref).unwrap(),
        };

        // Assert migrated and loaded state
        assert_eq!(block_state_p10.plt_list(&context_p10).len(), 2);
        let token1 = block_state_p10
            .token_by_id(&context_p10, &"token1".parse().unwrap())
            .unwrap()
            .unwrap();
        assert_eq!(
            token1.token_p9_base.token_circulating_supply(),
            RawTokenAmount(100)
        );
        assert_eq!(
            token1
                .token_p9_base
                .token_configuration(&context_p10)
                .unwrap(),
            configuration1
        );
        let value = token1
            .token_p9_base
            .mutable_key_value_state
            .lookup_value(&context_p10.loader, &[0, 1]);
        assert_eq!(value, Some(vec![0, 0]));
        let value = token1
            .token_p9_base
            .mutable_key_value_state
            .lookup_value(&context_p10.loader, &[0, 2]);
        assert_eq!(value, Some(vec![1, 1]));
        let token2 = block_state_p10
            .token_by_id(&context_p10, &"token2".parse().unwrap())
            .unwrap()
            .unwrap();
        assert_eq!(
            token2.token_p9_base.token_circulating_supply(),
            RawTokenAmount(0)
        );
        assert_eq!(
            token2
                .token_p9_base
                .token_configuration(&context_p10)
                .unwrap(),
            configuration2
        );
    }
}
