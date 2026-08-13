use crate::entity::block_state::p10::BlockStateP10;
use crate::entity::block_state::p11::BlockStateP11;
use crate::entity::protocol_level_tokens;
use crate::failure::BlockStateResult;
use crate::persistent::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::persistent::blob_store::{BlobStoreLoad, BlobStoreStore};
use crate::persistent::block_state::p11::PersistentBlockStateP11;
use crate::persistent::protocol_level_locks::p11::PersistentLocksP11;

/// Migrate the P10 block state to P11.
pub fn migrate_from_p10_to_p11(
    block_state_p10: BlockStateP10,
    from_store: &impl BlobStoreLoad,
    to_store: &mut (impl BlobStoreStore + BlobStoreLoad),
) -> BlockStateResult<BlockStateP11> {
    let new_tokens = protocol_level_tokens::migration::p10_to_p11::migrate_from_p10_to_p11(
        block_state_p10.persistent.tokens,
        from_store,
        to_store,
    )?;

    let new_persistent = PersistentBlockStateP11 {
        tokens: HashedCacheableRef::new(new_tokens),
        locks: HashedCacheableRef::new(PersistentLocksP11::default()),
    };

    Ok(BlockStateP11 {
        persistent: new_persistent,
    })
}

#[cfg(test)]
mod test {
    use crate::entity::block_state::migration;
    use crate::entity::block_state::p10::BlockStateP10;
    use crate::entity::entity_test_stub;
    use crate::entity::protocol_level_tokens::p11::Roles;
    use crate::persistent::protocol_level_tokens::p9::TokenConfiguration;
    use concordium_base::protocol_level_tokens::{TokenAdminRole, TokenModuleRef};
    use plt_scheduler_types::types::tokens::RawTokenAmount;

    /// Migrate block state from P10 to P11.
    #[test]
    fn test_migrate_p10_to_p11() {
        let mut context = entity_test_stub::new_stubbed_context();
        let mut block_state = BlockStateP10::default();

        let governance_account = context.external.create_account();

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
            .set_governance_account(&context, governance_account.account_index)
            .unwrap();
        token1
            .token_p9_base
            .set_token_circulating_supply(RawTokenAmount::from(100));
        token1
            .token_p9_base
            .set_deny_list_enabled(&context)
            .unwrap();
        token1.token_p9_base.set_burnable_enabled(&context).unwrap();
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
        let token_index2 = block_state
            .create_token(&context, configuration2.clone())
            .unwrap();
        let mut token2 = block_state.token_by_index(&context, token_index2).unwrap();
        token2
            .token_p9_base
            .set_governance_account(&context, governance_account.account_index)
            .unwrap();
        token2
            .token_p9_base
            .set_allow_list_enabled(&context)
            .unwrap();
        token2.token_p9_base.set_mintable_enabled(&context).unwrap();
        block_state.update_token(&context, token2).unwrap();

        // Migrate the block state
        let (migrated_context, migrated_block_state) =
            migration::test_utils::migrate_p10_to_p11(&mut context, block_state);

        // Assert on migrated block state
        assert_eq!(
            migrated_block_state
                .plt_list(&migrated_context)
                .unwrap()
                .len(),
            2
        );
        let token1 = migrated_block_state
            .token_by_id(&migrated_context, &"token1".parse().unwrap())
            .unwrap()
            .unwrap();
        assert_eq!(
            token1
                .token_p9_base
                .get_governance_account_index(&migrated_context)
                .unwrap(),
            governance_account.account_index
        );
        assert_eq!(
            token1.token_p9_base.token_circulating_supply(),
            RawTokenAmount::from(100)
        );
        assert!(token1.token_p9_base.has_deny_list(&migrated_context));
        assert!(token1.token_p9_base.is_burnable(&migrated_context));
        assert_eq!(
            token1
                .token_p9_base
                .get_token_name(&migrated_context)
                .unwrap(),
            "token1name"
        );
        assert_eq!(
            token1
                .token_p9_base
                .token_configuration(&migrated_context)
                .unwrap(),
            configuration1
        );
        let token2 = migrated_block_state
            .token_by_id(&migrated_context, &"token2".parse().unwrap())
            .unwrap()
            .unwrap();
        assert_eq!(
            token2
                .token_p9_base
                .get_governance_account_index(&migrated_context)
                .unwrap(),
            governance_account.account_index
        );
        assert_eq!(
            token2.token_p9_base.token_circulating_supply(),
            RawTokenAmount::from(0)
        );
        assert!(token2.token_p9_base.has_allow_list(&migrated_context));
        assert!(token2.token_p9_base.is_mintable(&migrated_context));
        assert_eq!(
            token2
                .token_p9_base
                .token_configuration(&migrated_context)
                .unwrap(),
            configuration2
        );

        // Assert that migrated tokens have had new roles added
        let new_roles1 = token1
            .get_account_roles(&migrated_context, governance_account.account_index)
            .unwrap();
        let mut expected_roles1 = Roles::none();
        expected_roles1.assign(TokenAdminRole::UpdateAdminRoles);
        expected_roles1.assign(TokenAdminRole::UpdateMetadata);
        expected_roles1.assign(TokenAdminRole::Pause);
        expected_roles1.assign(TokenAdminRole::Burn);
        expected_roles1.assign(TokenAdminRole::UpdateDenyList);
        assert_eq!(new_roles1, expected_roles1);
        let new_roles2 = token2
            .get_account_roles(&migrated_context, governance_account.account_index)
            .unwrap();
        let mut expected_roles2 = Roles::none();
        expected_roles2.assign(TokenAdminRole::UpdateAdminRoles);
        expected_roles2.assign(TokenAdminRole::UpdateMetadata);
        expected_roles2.assign(TokenAdminRole::Pause);
        expected_roles2.assign(TokenAdminRole::Mint);
        expected_roles2.assign(TokenAdminRole::UpdateAllowList);
        assert_eq!(new_roles2, expected_roles2);

        // Assert no locks
        assert_eq!(
            migrated_block_state
                .lock_list(&migrated_context)
                .unwrap_or_default(),
            vec![]
        );
    }
}
