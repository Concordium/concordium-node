//! Internal constants and utilities for token key-value state.

use crate::block_state::blob_store::BlobStoreLoad;
use crate::block_state::smart_contract_trie;
use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use crate::entity::protocol_level_tokens::state_key::roles::Roles;
use concordium_base::common;
use concordium_base::protocol_level_tokens::{
    MetadataUrl, TokenAdminRole, TokenAuthorizations, TokenRoleAuthorizations,
};
use concordium_base::{base::AccountIndex, common::Serial};

/// Little-endian prefix used to distinguish module state keys.
const MODULE_STATE_PREFIX: [u8; 2] = 0u16.to_le_bytes();

/// Little-endian prefix used to distinguish account state keys.
const ACCOUNT_STATE_PREFIX: [u8; 2] = 40307u16.to_le_bytes();
/// Little-endian prefix used to distinguish account role state keys.
///
/// Note the roles are stored separately from the remaining account state, to allow for iterating
/// the prefix.
const ACCOUNT_ROLES_STATE_PREFIX: [u8; 2] = 40308u16.to_le_bytes();

pub const STATE_KEY_NAME: &[u8] = b"name";
pub const STATE_KEY_METADATA: &[u8] = b"metadata";
pub const STATE_KEY_ALLOW_LIST: &[u8] = b"allowList";
pub const STATE_KEY_DENY_LIST: &[u8] = b"denyList";
pub const STATE_KEY_MINTABLE: &[u8] = b"mintable";
pub const STATE_KEY_BURNABLE: &[u8] = b"burnable";
pub const STATE_KEY_PAUSED: &[u8] = b"paused";
pub const STATE_KEY_GOVERNANCE_ACCOUNT: &[u8] = b"governanceAccount";

/// Construct a [`TokenStateKey`] for a module key. This prefixes the key to
/// distinguish it from other keys.
pub fn module_state_key(key: &[u8]) -> Vec<u8> {
    let mut module_key = Vec::with_capacity(MODULE_STATE_PREFIX.len() + key.len());
    module_key.extend_from_slice(&MODULE_STATE_PREFIX);
    module_key.extend_from_slice(key);
    module_key
}

/// Construct a key for the account section of the token state.
pub fn account_state_key(account_index: AccountIndex, key: &[u8]) -> Vec<u8> {
    let mut account_key =
        Vec::with_capacity(ACCOUNT_STATE_PREFIX.len() + size_of::<AccountIndex>() + key.len());
    account_key.extend_from_slice(&ACCOUNT_STATE_PREFIX);
    account_index.serial(&mut account_key);
    account_key.extend_from_slice(key);
    account_key
}

/// Construct a key for the account roles section of the token state.
pub fn account_roles_state_key(account_index: AccountIndex) -> Vec<u8> {
    let mut account_key =
        Vec::with_capacity(ACCOUNT_ROLES_STATE_PREFIX.len() + size_of::<AccountIndex>());
    account_key.extend_from_slice(&ACCOUNT_ROLES_STATE_PREFIX);
    account_index.serial(&mut account_key);
    account_key
}

/// Lookup a value from the account roles section of the token state.
fn get_account_roles_state(
    loader: &impl BlobStoreLoad,
    trie: &smart_contract_trie::MutableState,
    account: AccountIndex,
) -> BlockStateResult<Roles> {
    Roles::try_from_state_value(
        trie.lookup_value(loader, &account_roles_state_key(account))
            .as_deref(),
    )
    .map_err(|err| {
        BlockStateFailure::Invariant(format!(
            "Stored account authorization roles cannot be decoded: {}",
            err
        ))
    })
}

/// Update a value in the account section of the token state.
fn update_account_roles_state(
    loader: &impl BlobStoreLoad,
    trie: &mut smart_contract_trie::MutableState,
    account: AccountIndex,
    rules: Roles,
) -> BlockStateResult<()> {
    if let Some(value) = rules.into_state_value() {
        trie.insert_value(loader, &account_roles_state_key(account), value)
    } else {
        trie.delete_value(loader, &account_roles_state_key(account))
    }
}

/// Get the authorization roles for an account from state.
pub fn get_account_roles(
    context: &impl ReadTokenKeyValueState,
    account: AccountIndex,
) -> Result<Roles, TokenStateInvariantError> {
    get_account_roles_state(context, account)
}

/// Assign roles to an account in the state.
pub fn assign_account_roles<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    account: AccountIndex,
    assigned_roles: &[TokenAdminRole],
) -> Result<(), TokenStateInvariantError> {
    let mut roles = get_account_roles_state(context, account)?;
    for role in assigned_roles {
        roles.assign(*role)
    }
    update_account_roles_state(context, account, roles);
    Ok(())
}

/// Revoke roles of an account in the state.
pub fn revoke_account_roles<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    account: AccountIndex,
    revoked_roles: &[TokenAdminRole],
) -> Result<(), TokenStateInvariantError> {
    let mut roles = get_account_roles_state(context, account)?;
    for role in revoked_roles {
        roles.revoke(*role)
    }
    update_account_roles_state(context, account, roles);
    Ok(())
}

/// Get authorization roles and assigned accounts for the token.
pub fn get_token_authorizations<BSQ: BlockStateQuery>(
    context: &TokenQueryContext<'_, BSQ>,
) -> Result<TokenAuthorizations, TokenStateInvariantError> {
    let mut update_admin_roles = TokenRoleAuthorizations::default();
    let mut mint = TokenRoleAuthorizations::default();
    let mut burn = TokenRoleAuthorizations::default();
    let mut update_allow_list = TokenRoleAuthorizations::default();
    let mut update_deny_list = TokenRoleAuthorizations::default();
    let mut pause = TokenRoleAuthorizations::default();
    let mut update_metadata = TokenRoleAuthorizations::default();

    for (key, roles) in
        context.iter_token_state_prefix(&TokenStateKey(ACCOUNT_ROLES_STATE_PREFIX.into()))
    {
        let account_index_bytes =
            key.0
                .strip_prefix(&ACCOUNT_ROLES_STATE_PREFIX)
                .ok_or_else(|| {
                    TokenStateInvariantError(
                        "Iterator over account roles state produced invalid key".to_string(),
                    )
                })?;
        let account_index: AccountIndex = common::from_bytes_complete(account_index_bytes)
            .map_err(|err| {
                TokenStateInvariantError(format!(
                    "Stored account index in authorizations cannot be decoded: {}",
                    err
                ))
            })?;
        let account = context
            .block_state
            .account_by_index(account_index)
            .map_err(|err| {
                TokenStateInvariantError(format!(
                    "Stored account index in authorizations cannot be found: {}",
                    err
                ))
            })?
            .canonical_account_address;
        let roles = Roles::try_from_state_value(Some(&roles)).map_err(|err| {
            TokenStateInvariantError(format!(
                "Stored account authorization roles cannot be decoded: {}",
                err
            ))
        })?;
        for role in roles.iter_assigned() {
            match role {
                TokenAdminRole::UpdateAdminRoles => {
                    update_admin_roles.accounts.push(account.into())
                }
                TokenAdminRole::Mint => mint.accounts.push(account.into()),
                TokenAdminRole::Burn => burn.accounts.push(account.into()),
                TokenAdminRole::UpdateAllowList => update_allow_list.accounts.push(account.into()),
                TokenAdminRole::UpdateDenyList => update_deny_list.accounts.push(account.into()),
                TokenAdminRole::Pause => pause.accounts.push(account.into()),
                TokenAdminRole::UpdateMetadata => update_metadata.accounts.push(account.into()),
            }
        }
    }
    Ok(TokenAuthorizations {
        update_admin_roles: Some(update_admin_roles),
        mint: is_mintable(context).then_some(mint),
        burn: is_burnable(context).then_some(burn),
        update_allow_list: has_allow_list(context).then_some(update_allow_list),
        update_deny_list: has_deny_list(context).then_some(update_deny_list),
        pause: Some(pause),
        update_metadata: Some(update_metadata),
    })
}

#[cfg(test)]
mod test {
    use super::*;

    /// Test that the module state key is formed correctly
    #[test]
    fn test_module_state_key() {
        let key = module_state_key(&[1, 2, 3]);
        assert_eq!(key, vec![0, 0, 1, 2, 3]);
    }

    /// Test that the account state key is formed correctly
    #[test]
    fn test_account_state_key() {
        let key = account_state_key(AccountIndex::from(1u64), &[1, 2, 3]);
        assert_eq!(key, vec![115, 157, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3]);
    }
}
