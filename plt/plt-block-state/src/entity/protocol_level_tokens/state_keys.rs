//! Internal constants and utilities for token key-value state.

use concordium_base::{base::AccountIndex, common::Serial};

/// Little-endian prefix used to distinguish module state keys.
const MODULE_STATE_PREFIX: [u8; 2] = 0u16.to_le_bytes();

/// Little-endian prefix used to distinguish account state keys.
const ACCOUNT_STATE_PREFIX: [u8; 2] = 40307u16.to_le_bytes();

/// Little-endian prefix used to distinguish account role state keys.
///
/// Note the roles are stored separately from the remaining account state, to allow for iterating
/// the prefix.
pub(crate) const ACCOUNT_ROLES_STATE_PREFIX: [u8; 2] = 40308u16.to_le_bytes();

pub(crate) const STATE_KEY_NAME: &[u8] = b"name";
pub(crate) const STATE_KEY_METADATA: &[u8] = b"metadata";
pub(crate) const STATE_KEY_ALLOW_LIST: &[u8] = b"allowList";
pub(crate) const STATE_KEY_DENY_LIST: &[u8] = b"denyList";
pub(crate) const STATE_KEY_MINTABLE: &[u8] = b"mintable";
pub(crate) const STATE_KEY_BURNABLE: &[u8] = b"burnable";
pub(crate) const STATE_KEY_PAUSED: &[u8] = b"paused";
pub(crate) const STATE_KEY_GOVERNANCE_ACCOUNT: &[u8] = b"governanceAccount";

/// Construct a [`TokenStateKey`] for a module key. This prefixes the key to
/// distinguish it from other keys.
pub(crate) fn module_state_key(key: &[u8]) -> Vec<u8> {
    let mut module_key = Vec::with_capacity(MODULE_STATE_PREFIX.len() + key.len());
    module_key.extend_from_slice(&MODULE_STATE_PREFIX);
    module_key.extend_from_slice(key);
    module_key
}

/// Construct a key for the account section of the token state.
pub(crate) fn account_state_key(account_index: AccountIndex, key: &[u8]) -> Vec<u8> {
    let mut account_key =
        Vec::with_capacity(ACCOUNT_STATE_PREFIX.len() + size_of::<AccountIndex>() + key.len());
    account_key.extend_from_slice(&ACCOUNT_STATE_PREFIX);
    account_index.serial(&mut account_key);
    account_key.extend_from_slice(key);
    account_key
}

/// Construct a key for the account roles section of the token state.
pub(crate) fn account_roles_state_key(account_index: AccountIndex) -> Vec<u8> {
    let mut account_key =
        Vec::with_capacity(ACCOUNT_ROLES_STATE_PREFIX.len() + size_of::<AccountIndex>());
    account_key.extend_from_slice(&ACCOUNT_ROLES_STATE_PREFIX);
    account_index.serial(&mut account_key);
    account_key
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

    /// Test that the account roles state key is formed correctly
    #[test]
    fn test_account_state_key() {
        let key = account_roles_state_key(AccountIndex::from(1u64));
        assert_eq!(key, vec![115, 157, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3]);
    }
}
