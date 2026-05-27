//! Internal constants and utilities for token key-value state.

use super::roles::Roles;
use crate::token_context::{TokenOperationContext, TokenQueryContext};
use crate::token_module::errors::TokenStateInvariantError;
use concordium_base::common;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::{
    MetadataUrl, TokenAdminRole, TokenAuthorizations, TokenRoleAuthorizations,
};
use concordium_base::{base::AccountIndex, common::Serial};
use plt_block_state::block_state_interface::{
    BlockStateOperations, BlockStateQuery, TokenStateKey, TokenStateValue,
};
use plt_block_state::utils;
use plt_scheduler_types::types::tokens::RawTokenAmount;

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
pub const ACCOUNT_STATE_KEY_QUANTA: &[u8] = b"quanta";

/// Minimal read-only access to token key-value state.
///
/// Allows defining functions which works for both [`TokenQueryContext`] and
/// [`TokenOperationContext`].
pub trait ReadTokenKeyValueState {
    /// Lookup a key in the token key-value state.
    fn lookup_token_state_value(&self, key: &TokenStateKey) -> Option<TokenStateValue>;

    /// Get an iterator over key-value pairs that share the given prefix.
    fn iter_token_state_prefix(
        &self,
        prefix: &TokenStateKey,
    ) -> impl Iterator<Item = (TokenStateKey, TokenStateValue)>;
}

impl<'a, BSQ> ReadTokenKeyValueState for TokenQueryContext<'a, BSQ>
where
    BSQ: BlockStateQuery,
{
    fn lookup_token_state_value(&self, key: &TokenStateKey) -> Option<TokenStateValue> {
        self.block_state
            .lookup_token_state_value(self.token_module_state, key)
    }

    fn iter_token_state_prefix(
        &self,
        prefix: &TokenStateKey,
    ) -> impl Iterator<Item = (TokenStateKey, TokenStateValue)> {
        self.block_state
            .iter_token_state_prefix(self.token_module_state, prefix)
    }
}

impl<'a, BSO> ReadTokenKeyValueState for TokenOperationContext<'a, BSO>
where
    BSO: BlockStateOperations,
{
    fn lookup_token_state_value(&self, key: &TokenStateKey) -> Option<TokenStateValue> {
        self.block_state
            .lookup_token_state_value(self.token_module_state, key)
    }

    fn iter_token_state_prefix(
        &self,
        prefix: &TokenStateKey,
    ) -> impl Iterator<Item = (TokenStateKey, TokenStateValue)> {
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
    TokenStateKey(module_key)
}

/// Get value from the token module state at the given key.
fn get_module_state(context: &impl ReadTokenKeyValueState, key: &[u8]) -> Option<TokenStateValue> {
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
    TokenStateKey(account_key)
}

/// Lookup a value from the account section of the token state.
fn lookup_account_state(
    context: &impl ReadTokenKeyValueState,
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
    TokenStateKey(account_key)
}

fn account_quanta_state_key(lock_id: &LockId) -> Vec<u8> {
    let mut locked_balance_key =
        Vec::with_capacity(ACCOUNT_STATE_KEY_QUANTA.len() + size_of::<LockId>());
    locked_balance_key.extend_from_slice(ACCOUNT_STATE_KEY_QUANTA);
    lock_id.serial(&mut locked_balance_key);
    locked_balance_key
}

/// Lookup a value from the account roles section of the token state.
fn get_account_roles_state(
    context: &impl ReadTokenKeyValueState,
    account: AccountIndex,
) -> Result<Roles, TokenStateInvariantError> {
    Roles::try_from_state_value(
        context
            .lookup_token_state_value(&account_roles_state_key(account))
            .as_ref(),
    )
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
pub fn is_paused(context: &impl ReadTokenKeyValueState) -> bool {
    get_module_state(context, STATE_KEY_PAUSED).is_some()
}

/// Get whether the token has allow lists enabled.
pub fn has_allow_list(context: &impl ReadTokenKeyValueState) -> bool {
    get_module_state(context, STATE_KEY_ALLOW_LIST).is_some()
}

/// Enabled 'allowList' feature for the token.
pub fn set_allow_list_enabled<BSO: BlockStateOperations>(context: &mut TokenOperationContext<BSO>) {
    set_module_state(context, STATE_KEY_ALLOW_LIST, Some(TokenStateValue(vec![])));
}

/// Get whether the token has deny lists enabled.
pub fn has_deny_list(context: &impl ReadTokenKeyValueState) -> bool {
    get_module_state(context, STATE_KEY_DENY_LIST).is_some()
}

/// Enabled 'DenyList' feature for the token.
pub fn set_deny_list_enabled<BSO: BlockStateOperations>(context: &mut TokenOperationContext<BSO>) {
    set_module_state(context, STATE_KEY_DENY_LIST, Some(TokenStateValue(vec![])));
}

/// Get whether the token allows minting.
pub fn is_mintable(context: &impl ReadTokenKeyValueState) -> bool {
    get_module_state(context, STATE_KEY_MINTABLE).is_some()
}

/// Enabled 'Mintable' feature for the token.
pub fn set_mintable_enabled<BSO: BlockStateOperations>(context: &mut TokenOperationContext<BSO>) {
    set_module_state(context, STATE_KEY_MINTABLE, Some(TokenStateValue(vec![])));
}

/// Get whether the token allows burning.
pub fn is_burnable(context: &impl ReadTokenKeyValueState) -> bool {
    get_module_state(context, STATE_KEY_BURNABLE).is_some()
}

/// Enabled 'Burnable' feature for the token.
pub fn set_burnable_enabled<BSO: BlockStateOperations>(context: &mut TokenOperationContext<BSO>) {
    set_module_state(context, STATE_KEY_BURNABLE, Some(TokenStateValue(vec![])));
}

/// Set the token governance account in module state.
pub fn set_governance_account<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    account: AccountIndex,
) {
    set_module_state(
        context,
        STATE_KEY_GOVERNANCE_ACCOUNT,
        Some(TokenStateValue(common::to_bytes(&account))),
    );
}

/// Set the token governance account in module state.
pub fn set_token_name<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    name: String,
) {
    set_module_state(context, STATE_KEY_NAME, Some(TokenStateValue(name.into())));
}

/// Get the name of the token.
pub fn get_token_name(
    context: &impl ReadTokenKeyValueState,
) -> Result<String, TokenStateInvariantError> {
    get_module_state(context, STATE_KEY_NAME)
        .ok_or_else(|| TokenStateInvariantError("Name not present".to_string()))
        .and_then(|value| {
            String::from_utf8(value.0).map_err(|err| {
                TokenStateInvariantError(format!("Stored name is invalid UTF-8: {}", err))
            })
        })
}

/// Get the URL metadata of the token.
pub fn get_metadata(
    context: &impl ReadTokenKeyValueState,
) -> Result<MetadataUrl, TokenStateInvariantError> {
    let metadata_cbor = get_module_state(context, STATE_KEY_METADATA)
        .ok_or_else(|| TokenStateInvariantError("Metadata not present".to_string()))?;
    let metadata: MetadataUrl = utils::cbor_decode(metadata_cbor.0).map_err(|err| {
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
    set_module_state(
        context,
        STATE_KEY_METADATA,
        Some(TokenStateValue(encoded_metadata)),
    );
}

/// Get the account index of the governance account for the token.
pub fn get_governance_account_index(
    context: &impl ReadTokenKeyValueState,
) -> Result<AccountIndex, TokenStateInvariantError> {
    let governance_account_index = AccountIndex::from(
        get_module_state(context, STATE_KEY_GOVERNANCE_ACCOUNT)
            .ok_or_else(|| TokenStateInvariantError("Governance account not present".to_string()))
            .and_then(|value| {
                common::from_bytes_complete::<u64>(value.0.as_slice()).map_err(|err| {
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

/// Sets the paused state of the token module.
pub fn set_paused<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    value: bool,
) {
    let state_value = value.then_some(TokenStateValue(vec![]));
    set_module_state(context, STATE_KEY_PAUSED, state_value)
}

/// Get the allow-list state for the account at the given account.
pub fn get_allow_list_for(context: &impl ReadTokenKeyValueState, account: AccountIndex) -> bool {
    lookup_account_state(context, account, STATE_KEY_ALLOW_LIST).is_some()
}

/// Set the allow-list state for the account at the given account.
pub fn set_allow_list_for<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    account: AccountIndex,
    value: bool,
) {
    let state_value = value.then_some(TokenStateValue(vec![]));
    update_account_state(context, account, STATE_KEY_ALLOW_LIST, state_value)
}

/// Get the deny-list state for the account at the given account.
pub fn get_deny_list_for(context: &impl ReadTokenKeyValueState, account: AccountIndex) -> bool {
    lookup_account_state(context, account, STATE_KEY_DENY_LIST).is_some()
}

/// Set the deny-list state for the account at the given account.
pub fn set_deny_list_for<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    account: AccountIndex,
    value: bool,
) {
    let state_value = value.then_some(TokenStateValue(vec![]));
    update_account_state(context, account, STATE_KEY_DENY_LIST, state_value)
}

/// Get the locked balance for the given account and lock.
pub fn get_locked_balance_for(
    context: &impl ReadTokenKeyValueState,
    account: AccountIndex,
    lock: &LockId,
) -> Result<RawTokenAmount, TokenStateInvariantError> {
    let Some(value) = lookup_account_state(context, account, &account_quanta_state_key(lock))
    else {
        return Ok(RawTokenAmount(0));
    };
    decode_locked_balance(value)
}

/// Get the locked balances recorded in token-module account state for the given
/// account.
pub fn get_locked_balances_for_account(
    context: &impl ReadTokenKeyValueState,
    account: AccountIndex,
) -> Result<Vec<(LockId, RawTokenAmount)>, TokenStateInvariantError> {
    let prefix = account_state_key(account, ACCOUNT_STATE_KEY_QUANTA);
    context
        .iter_token_state_prefix(&prefix)
        .map(|(key, value)| {
            let lock = common::from_bytes_complete(&key.0[prefix.0.len()..]).map_err(|err| {
                TokenStateInvariantError(format!("Stored lock id cannot be decoded: {}", err))
            })?;
            let amount = decode_locked_balance(value)?;
            Ok((lock, amount))
        })
        .collect()
}

fn decode_locked_balance(
    value: TokenStateValue,
) -> Result<RawTokenAmount, TokenStateInvariantError> {
    common::from_bytes_complete(value.0).map_err(|err| {
        TokenStateInvariantError(format!("Stored locked balance cannot be decoded: {}", err))
    })
}

/// Set the locked balance for the given account and lock.
pub fn set_locked_balance_for<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<BSO>,
    account: AccountIndex,
    lock: &LockId,
    amount: RawTokenAmount,
) {
    let state_value = if amount == RawTokenAmount(0) {
        None
    } else {
        Some(TokenStateValue(common::to_bytes(&amount)))
    };
    update_account_state(
        context,
        account,
        &account_quanta_state_key(lock),
        state_value,
    )
}

#[cfg(test)]
mod test {
    use super::*;

    fn example_lock_id() -> LockId {
        LockId {
            account_index: 7,
            sequence_number: 11,
            creation_order: 3,
        }
    }

    /// Test that the module state key is formed correctly
    #[test]
    fn test_module_state_key() {
        let key = module_state_key(&[1, 2, 3]);
        assert_eq!(key, TokenStateKey(vec![0, 0, 1, 2, 3]));
    }

    /// Test that the account state key is formed correctly
    #[test]
    fn test_account_state_key() {
        let key = account_state_key(AccountIndex::from(1u64), &[1, 2, 3]);
        assert_eq!(
            key,
            TokenStateKey(vec![115, 157, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3])
        );
    }

    #[test]
    fn test_account_quanta_state_key() {
        let account = AccountIndex::from(1u64);
        let lock_id = example_lock_id();

        let mut expected = Vec::new();
        expected.extend_from_slice(&ACCOUNT_STATE_PREFIX);
        account.serial(&mut expected);
        expected.extend_from_slice(ACCOUNT_STATE_KEY_QUANTA);
        lock_id.serial(&mut expected);

        let key = account_state_key(account, &account_quanta_state_key(&lock_id));
        assert_eq!(key, TokenStateKey(expected));
    }
}
