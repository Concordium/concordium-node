//! Internal constants and utilities for token module state.

use crate::token_module::TokenModuleStateInvariantError;
use crate::util;
use concordium_base::base::AccountIndex;
use concordium_base::common;
use concordium_base::protocol_level_tokens::MetadataUrl;
use plt_scheduler_interface::token_kernel_interface::{
    TokenKernelOperations, TokenKernelQueries, TokenStateKey, TokenStateValue,
};

/// Little-endian prefix used to distinguish module state keys.
const MODULE_STATE_PREFIX: [u8; 2] = 0u16.to_le_bytes();

pub const STATE_KEY_NAME: &[u8] = b"name";
pub const STATE_KEY_METADATA: &[u8] = b"metadata";
pub const STATE_KEY_ALLOW_LIST: &[u8] = b"allowList";
pub const STATE_KEY_DENY_LIST: &[u8] = b"denyList";
pub const STATE_KEY_MINTABLE: &[u8] = b"mintable";
pub const STATE_KEY_BURNABLE: &[u8] = b"burnable";
pub const STATE_KEY_PAUSED: &[u8] = b"paused";
pub const STATE_KEY_GOVERNANCE_ACCOUNT: &[u8] = b"governanceAccount";

/// Extension trait for [`TokenKernelOperations`] to provide convenience wrappers for
/// module state updates.
pub trait KernelOperationsExt: TokenKernelOperations {
    /// Set or clear a value in the token module state at the corresponding key.
    fn set_module_state(&mut self, key: &[u8], value: Option<TokenStateValue>) {
        self.set_token_state_value(module_state_key(key), value);
    }
}

impl<T: TokenKernelOperations> KernelOperationsExt for T {}

/// Extension trait for [`TokenKernelQueries`] to provide convenience wrappers for
/// module state access.
pub trait KernelQueriesExt: TokenKernelQueries {
    /// Get value from the token module state at the given key.
    fn get_module_state(&self, key: &[u8]) -> Option<TokenStateValue> {
        self.lookup_token_state_value(module_state_key(key))
    }
}

impl<T: TokenKernelQueries> KernelQueriesExt for T {}

/// Construct a [`TokenStateKey`] for a module key. This prefixes the key to
/// distinguish it from other keys.
fn module_state_key(key: &[u8]) -> TokenStateKey {
    let mut module_key = Vec::with_capacity(MODULE_STATE_PREFIX.len() + key.len());
    module_key.extend_from_slice(&MODULE_STATE_PREFIX);
    module_key.extend_from_slice(key);
    module_key
}

/// Get whether the balance-affecting operations on the token are currently
/// paused.
pub fn is_paused(kernel: &impl TokenKernelQueries) -> bool {
    kernel.get_module_state(STATE_KEY_PAUSED).is_some()
}

/// Get whether the token has allow lists enabled.
pub fn has_allow_list(kernel: &impl TokenKernelQueries) -> bool {
    kernel.get_module_state(STATE_KEY_ALLOW_LIST).is_some()
}

/// Get whether the token has deny lists enabled.
pub fn has_deny_list(kernel: &impl TokenKernelQueries) -> bool {
    kernel.get_module_state(STATE_KEY_DENY_LIST).is_some()
}

/// Get whether the token allows minting.
pub fn is_mintable(kernel: &impl TokenKernelQueries) -> bool {
    kernel.get_module_state(STATE_KEY_MINTABLE).is_some()
}

/// Get whether the token allows burning.
pub fn is_burnable(kernel: &impl TokenKernelQueries) -> bool {
    kernel.get_module_state(STATE_KEY_BURNABLE).is_some()
}

/// Get the name of the token.
pub fn get_name(
    kernel: &impl TokenKernelQueries,
) -> Result<String, TokenModuleStateInvariantError> {
    kernel
        .get_module_state(STATE_KEY_NAME)
        .ok_or_else(|| TokenModuleStateInvariantError("Name not present".to_string()))
        .and_then(|value| {
            String::from_utf8(value).map_err(|err| {
                TokenModuleStateInvariantError(format!("Stored name is invalid UTF8: {}", err))
            })
        })
}

/// Get the URL metadata of the token.
pub fn get_metadata(
    kernel: &impl TokenKernelQueries,
) -> Result<MetadataUrl, TokenModuleStateInvariantError> {
    let metadata_cbor = kernel
        .get_module_state(STATE_KEY_METADATA)
        .ok_or_else(|| TokenModuleStateInvariantError("Metadata not present".to_string()))?;
    let metadata: MetadataUrl = util::cbor_decode(metadata_cbor).map_err(|err| {
        TokenModuleStateInvariantError(format!("Stored metadata CBOR not decodable: {}", err))
    })?;
    Ok(metadata)
}

/// Get the account index of the governance account for the token.
pub fn get_governance_account_index(
    kernel: &impl TokenKernelQueries,
) -> Result<AccountIndex, TokenModuleStateInvariantError> {
    let governance_account_index = AccountIndex::from(
        kernel
            .get_module_state(STATE_KEY_GOVERNANCE_ACCOUNT)
            .ok_or_else(|| {
                TokenModuleStateInvariantError("Governance account not present".to_string())
            })
            .and_then(|value| {
                common::from_bytes::<u64, _>(&mut value.as_slice()).map_err(|err| {
                    TokenModuleStateInvariantError(format!(
                        "Stored governance account index cannot be decoded: {}",
                        err
                    ))
                })
            })?,
    );
    Ok(governance_account_index)
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
}
