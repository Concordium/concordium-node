//! Internal constants and utilities for token key-value state.

use super::roles::Roles;
use super::util;
use crate::token_context::{TokenOperationContext, TokenQueryContext};
use crate::token_module::errors::TokenStateInvariantError;
use concordium_base::common;
use concordium_base::protocol_level_tokens::{
    MetadataUrl, TokenAdminRole, TokenAuthorizations, TokenRoleAuthorizations,
};
use concordium_base::{base::AccountIndex, common::Serial};
use plt_block_state::block_state::types::{TokenStateKey, TokenStateValue};
use plt_block_state::block_state_interface::{BlockStateOperations, BlockStateQuery};

/// Little-endian prefix used to distinguish module state keys.
const MODULE_STATE_PREFIX: [u8; 2] = 0u16.to_le_bytes();

/// Little-endian prefix used to distinguish account state keys.
const ACCOUNT_STATE_PREFIX: [u8; 2] = 40307u16.to_le_bytes();
/// Little-endian prefix used to distinguish account role state keys.
///
/// Note the roles are stored separately from the remaining account state, to allow for iterating
/// the prefix.
const ACCOUNT_ROLES_STATE_PREFIX: [u8; 2] = 40308u16.to_le_bytes();

const STATE_KEY_NAME: &[u8] = b"name";
const STATE_KEY_METADATA: &[u8] = b"metadata";
const STATE_KEY_ALLOW_LIST: &[u8] = b"allowList";
const STATE_KEY_DENY_LIST: &[u8] = b"denyList";
const STATE_KEY_MINTABLE: &[u8] = b"mintable";
const STATE_KEY_BURNABLE: &[u8] = b"burnable";
const STATE_KEY_PAUSED: &[u8] = b"paused";
const STATE_KEY_GOVERNANCE_ACCOUNT: &[u8] = b"governanceAccount";

/// Minimal read-only access to token key-value state.
///
/// Allows defining functions which works for both [`TokenQueryContext`] and
/// [`TokenOperationContext`].
pub trait ReadTokenState {
    /// Lookup a key in the token key-value state.
    fn lookup_token_state_value(&self, key: &TokenStateKey) -> Option<TokenStateValue>;

    /// Get an iterator over key-value pairs that share the given prefix.
    fn iter_token_state_prefix(
        &self,
        prefix: TokenStateKey,
    ) -> impl Iterator<Item = (&TokenStateKey, &TokenStateValue)>;
}

impl<'a, BSQ> ReadTokenState for TokenQueryContext<'a, BSQ>
where
    BSQ: BlockStateQuery,
{
    fn lookup_token_state_value(&self, key: &TokenStateKey) -> Option<TokenStateValue> {
        self.block_state
            .lookup_token_state_value(self.token_module_state, key)
    }

    fn iter_token_state_prefix(
        &self,
        prefix: TokenStateKey,
    ) -> impl Iterator<Item = (&TokenStateKey, &TokenStateValue)> {
        self.block_state
            .iter_token_state_prefix(self.token_module_state, prefix)
    }
}

impl<'a, BSO> ReadTokenState for TokenOperationContext<'a, BSO>
where
    BSO: BlockStateOperations,
{
    fn lookup_token_state_value(&self, key: &TokenStateKey) -> Option<TokenStateValue> {
        self.block_state
            .lookup_token_state_value(self.token_module_state, key)
    }

    fn iter_token_state_prefix(
        &self,
        prefix: TokenStateKey,
    ) -> impl Iterator<Item = (&TokenStateKey, &TokenStateValue)> {
        self.block_state
            .iter_token_state_prefix(self.token_module_state, prefix)
    }
}

/// Construct a [`TokenStateKey`] for a module key. This prefixes the key to
/// distinguish it from other keys.
fn module_state_key(key: &[u8]) -> TokenStateKey {
    let mut module_key = Vec::with_capacity(MODULE_STATE_PREFIX.len() + key.len());
    module_key.extend_from_slice(&MODULE_STATE_PREFIX);
    module_key.extend_from_slice(key);
    module_key
}

/// Get value from the token module state at the given key.
fn get_module_state(context: &impl ReadTokenState, key: &[u8]) -> Option<TokenStateValue> {
    context.lookup_token_state_value(&module_state_key(key))
}

/// Set or clear a value in the token module state at the corresponding key.
fn set_module_state<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<'_, BSO>,
    key: &[u8],
    value: Option<TokenStateValue>,
) {
    context.update_token_state_value(module_state_key(key), value);
}

/// Construct a key for the account section of the token state.
fn account_state_key(account_index: AccountIndex, key: &[u8]) -> TokenStateKey {
    let mut account_key =
        Vec::with_capacity(ACCOUNT_STATE_PREFIX.len() + size_of::<AccountIndex>() + key.len());
    account_key.extend_from_slice(&ACCOUNT_STATE_PREFIX);
    account_index.serial(&mut account_key);
    account_key.extend_from_slice(key);
    account_key
}

/// Lookup a value from the account section of the token state.
fn lookup_account_state(
    context: &impl ReadTokenState,
    account: AccountIndex,
    key: &[u8],
) -> Option<TokenStateValue> {
    context.lookup_token_state_value(&account_state_key(account, key))
}

/// Update a value in the account section of the token state.
fn update_account_state<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    account: AccountIndex,
    key: &[u8],
    value: Option<TokenStateValue>,
) {
    context.update_token_state_value(account_state_key(account, key), value);
}

/// Construct a key for the account roles section of the token state.
fn account_roles_state_key(account_index: AccountIndex) -> TokenStateKey {
    let mut account_key =
        Vec::with_capacity(ACCOUNT_ROLES_STATE_PREFIX.len() + size_of::<AccountIndex>());
    account_key.extend_from_slice(&ACCOUNT_ROLES_STATE_PREFIX);
    account_index.serial(&mut account_key);
    account_key
}

/// Lookup a value from the account roles section of the token state.
fn get_account_roles_state(
    context: &impl ReadTokenState,
    account: AccountIndex,
) -> Result<Roles, TokenStateInvariantError> {
    Roles::try_from_state_value(context.lookup_token_state_value(&account_roles_state_key(account)))
        .map_err(|err| {
            TokenStateInvariantError(format!(
                "Stored account authorization roles cannot be decoded: {}",
                err
            ))
        })
}

/// Update a value in the account section of the token state.
fn update_account_roles_state<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    account: AccountIndex,
    value: Roles,
) {
    context.update_token_state_value(account_roles_state_key(account), value.into_state_value());
}

/// Get whether the balance-affecting operations on the token are currently
/// paused.
pub fn is_paused(context: &impl ReadTokenState) -> bool {
    get_module_state(context, STATE_KEY_PAUSED).is_some()
}

/// Get whether the token has allow lists enabled.
pub fn has_allow_list(context: &impl ReadTokenState) -> bool {
    get_module_state(context, STATE_KEY_ALLOW_LIST).is_some()
}

/// Enabled 'allowList' feature for the token.
pub fn set_allow_list_enabled<BSO: BlockStateOperations>(context: &mut TokenOperationContext<BSO>) {
    set_module_state(context, STATE_KEY_ALLOW_LIST, Some(vec![]));
}

/// Get whether the token has deny lists enabled.
pub fn has_deny_list(context: &impl ReadTokenState) -> bool {
    get_module_state(context, STATE_KEY_DENY_LIST).is_some()
}

/// Enabled 'DenyList' feature for the token.
pub fn set_deny_list_enabled<BSO: BlockStateOperations>(context: &mut TokenOperationContext<BSO>) {
    set_module_state(context, STATE_KEY_DENY_LIST, Some(vec![]));
}

/// Get whether the token allows minting.
pub fn is_mintable(context: &impl ReadTokenState) -> bool {
    get_module_state(context, STATE_KEY_MINTABLE).is_some()
}

/// Enabled 'Mintable' feature for the token.
pub fn set_mintable_enabled<BSO: BlockStateOperations>(context: &mut TokenOperationContext<BSO>) {
    set_module_state(context, STATE_KEY_MINTABLE, Some(vec![]));
}

/// Get whether the token allows burning.
pub fn is_burnable(context: &impl ReadTokenState) -> bool {
    get_module_state(context, STATE_KEY_BURNABLE).is_some()
}

/// Enabled 'Burnable' feature for the token.
pub fn set_burnable_enabled<BSO: BlockStateOperations>(context: &mut TokenOperationContext<BSO>) {
    set_module_state(context, STATE_KEY_BURNABLE, Some(vec![]));
}

/// Set the token governance account in module state.
pub fn set_governance_account<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    account: AccountIndex,
) {
    set_module_state(
        context,
        STATE_KEY_GOVERNANCE_ACCOUNT,
        Some(common::to_bytes(&account)),
    );
}

/// Set the token governance account in module state.
pub fn set_token_name<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    name: String,
) {
    set_module_state(context, STATE_KEY_NAME, Some(name.into()));
}

/// Get the name of the token.
pub fn get_token_name(context: &impl ReadTokenState) -> Result<String, TokenStateInvariantError> {
    get_module_state(context, STATE_KEY_NAME)
        .ok_or_else(|| TokenStateInvariantError("Name not present".to_string()))
        .and_then(|value| {
            String::from_utf8(value).map_err(|err| {
                TokenStateInvariantError(format!("Stored name is invalid UTF-8: {}", err))
            })
        })
}

/// Get the URL metadata of the token.
pub fn get_metadata(
    context: &impl ReadTokenState,
) -> Result<MetadataUrl, TokenStateInvariantError> {
    let metadata_cbor = get_module_state(context, STATE_KEY_METADATA)
        .ok_or_else(|| TokenStateInvariantError("Metadata not present".to_string()))?;
    let metadata: MetadataUrl = util::cbor_decode(metadata_cbor).map_err(|err| {
        TokenStateInvariantError(format!("Stored metadata CBOR not decodable: {}", err))
    })?;
    Ok(metadata)
}

/// Set the metadata URL.
pub fn set_metadata_url<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    metadata: &MetadataUrl,
) {
    let encoded_metadata = common::cbor::cbor_encode(metadata);
    set_module_state(context, STATE_KEY_METADATA, Some(encoded_metadata));
}

/// Get the account index of the governance account for the token.
pub fn get_governance_account_index(
    context: &impl ReadTokenState,
) -> Result<AccountIndex, TokenStateInvariantError> {
    let governance_account_index = AccountIndex::from(
        get_module_state(context, STATE_KEY_GOVERNANCE_ACCOUNT)
            .ok_or_else(|| TokenStateInvariantError("Governance account not present".to_string()))
            .and_then(|value| {
                common::from_bytes_complete::<u64>(&mut value.as_slice()).map_err(|err| {
                    TokenStateInvariantError(format!(
                        "Stored governance account index cannot be decoded: {}",
                        err
                    ))
                })
            })?,
    );
    Ok(governance_account_index)
}

/// Get the authorization roles for an account from state.
pub fn get_account_roles(
    context: &impl ReadTokenState,
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

    for (key, roles) in context.iter_token_state_prefix(ACCOUNT_ROLES_STATE_PREFIX.into()) {
        let account_index_bytes =
            key.strip_prefix(&ACCOUNT_ROLES_STATE_PREFIX)
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
        let roles = Roles::try_from_state_value(Some(roles.clone())).map_err(|err| {
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

/// Sets the paused state of the token module.
pub fn set_paused<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    value: bool,
) {
    let state_value = value.then_some(vec![]);
    set_module_state(context, STATE_KEY_PAUSED, state_value)
}

/// Get the allow-list state for the account at the given account.
pub fn get_allow_list_for(context: &impl ReadTokenState, account: AccountIndex) -> bool {
    lookup_account_state(context, account, STATE_KEY_ALLOW_LIST).is_some()
}

/// Set the allow-list state for the account at the given account.
pub fn set_allow_list_for<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    account: AccountIndex,
    value: bool,
) {
    let state_value = value.then_some(vec![]);
    update_account_state(context, account, STATE_KEY_ALLOW_LIST, state_value)
}

/// Get the deny-list state for the account at the given account.
pub fn get_deny_list_for(context: &impl ReadTokenState, account: AccountIndex) -> bool {
    lookup_account_state(context, account, STATE_KEY_DENY_LIST).is_some()
}

/// Set the deny-list state for the account at the given account.
pub fn set_deny_list_for<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    account: AccountIndex,
    value: bool,
) {
    let state_value = value.then_some(vec![]);
    update_account_state(context, account, STATE_KEY_DENY_LIST, state_value)
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
