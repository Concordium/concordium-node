use crate::entity::protocol_level_tokens::p9::TokenP9Base;
use crate::entity::protocol_level_tokens::p11::{TokenP11, UNIVERSAL_ROLES};
use crate::entity::{EntityContext, EntityContextTypesWitness};
use crate::external::test_stub::UnreachableExternalBlockState;
use crate::failure::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::persistent::blob_store::{BlobStoreLoad, BlobStoreMovable, BlobStoreStore};
use crate::persistent::lfmb_tree::LfmbTree;
use crate::persistent::protocol_level_tokens::p9::PersistentTokensP9;
use concordium_base::protocol_level_tokens::TokenAdminRole;

/// Context initialized during migration with no access to external block state (will panic if accessed).
type MigrationEntityContextNoExternal<'a, L> =
    EntityContext<EntityContextTypesWitness<UnreachableExternalBlockState, &'a mut L>>;

/// Migrate the P10 block state to P11.
pub fn migrate_from_p10_to_p11(
    mut persistent_tokens: PersistentTokensP9,
    from_store: &impl BlobStoreLoad,
    to_store: &mut (impl BlobStoreStore + BlobStoreLoad),
) -> BlockStateResult<PersistentTokensP9> {
    let to_context = MigrationEntityContextNoExternal {
        external: UnreachableExternalBlockState,
        store: to_store,
    };

    let mut new_tokens = LfmbTree::empty();
    for item in persistent_tokens.tokens.values(from_store) {
        let (token_index, persistent_token) = item?;

        let moved_persistent_token =
            persistent_token.move_blob_store(from_store, to_context.store)?;

        let mut new_token = TokenP11 {
            token_p9_base: TokenP9Base {
                token_index,
                mutable_key_value_state: moved_persistent_token
                    .key_value_state
                    .value(to_context.store)?
                    .thaw(),
                persistent: moved_persistent_token,
            },
        };

        // The governance account should hold every role, except for disabled features, so we build a
        // list of every enabled role and the mandatory roles.
        let mut enabled_roles = Vec::from(UNIVERSAL_ROLES);

        if new_token.token_p9_base.has_allow_list(&to_context) {
            enabled_roles.push(TokenAdminRole::UpdateAllowList);
        }
        if new_token.token_p9_base.has_deny_list(&to_context) {
            enabled_roles.push(TokenAdminRole::UpdateDenyList);
        }
        if new_token.token_p9_base.is_mintable(&to_context) {
            enabled_roles.push(TokenAdminRole::Mint);
        }
        if new_token.token_p9_base.is_burnable(&to_context) {
            enabled_roles.push(TokenAdminRole::Burn);
        }

        let governance_account_index = new_token
            .token_p9_base
            .get_governance_account_index(&to_context)?;
        new_token.assign_account_roles(&to_context, governance_account_index, &enabled_roles)?;

        if new_token.token_p9_base.mutable_key_value_state.is_dirty() {
            new_token.token_p9_base.persistent.key_value_state = HashedCacheableRef::new(
                new_token
                    .token_p9_base
                    .mutable_key_value_state
                    .freeze(to_context.store),
            );
        }

        let new_token_index;
        (new_token_index, new_tokens) =
            new_tokens.insert_value(to_context.store, new_token.token_p9_base.persistent)?;
        if new_token_index != token_index {
            return Err(BlockStateFailure::Invariant(format!(
                "token index changes from {:?} to {:?} during P10 to P11 migration",
                token_index, new_token_index
            )));
        }
    }

    persistent_tokens.tokens = new_tokens;

    Ok(persistent_tokens)
}
