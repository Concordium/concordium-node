use std::collections::BTreeMap;

use concordium_base::common::Serialize;
use concordium_base::constants::SHA256;
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler_types::types::tokens::RawTokenAmount;

use crate::block_state_interface::TokenNotFoundByIdError;

use super::types::{TokenConfiguration, TokenIndex, TokenStateKey, TokenStateValue};
use super::{BlockStateOperations, PltBlockStateHash, blob_store};

/// Block state providing the various block state operations.
#[derive(Debug, Clone)]
pub struct PltBlockStateP10 {
    // todo implement real block state as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
    /// Simplistic state that is used as a temporary implementation of the block state
    pub tokens: Tokens,
}

impl blob_store::Loadable for PltBlockStateP10 {
    fn load(
        _loader: &mut impl blob_store::BackingStoreLoad,
        _source: impl AsRef<[u8]>,
    ) -> Result<Self, blob_store::DecodeError> {
        todo!()
    }
}

impl BlockStateOperations for PltBlockStateP10 {
    fn empty() -> Self {
        Self {
            tokens: Default::default(),
        }
    }

    fn hash(&self, _loader: &mut impl blob_store::BackingStoreLoad) -> PltBlockStateHash {
        // todo do real implementation as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
        self.tokens.hash()
    }

    fn store_update(
        &self,
        _storer: &mut impl blob_store::BackingStoreStore,
    ) -> blob_store::Reference {
        todo!()
    }

    fn cache(&mut self, _loader: &mut impl blob_store::BackingStoreLoad) {
        todo!()
    }
}

/// Simplistic implementation of the block state that serializes as a flat sequence of bytes.
// todo do real implementation as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
#[derive(Debug, Default, Clone, Serialize)]
pub struct Tokens {
    tokens: Vec<Token>,
}

#[derive(Debug, Clone, Serialize)]
struct Token {
    key_value_state: SimplisticTokenKeyValueState,
    configuration: TokenConfiguration,
    circulating_supply: RawTokenAmount,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct SimplisticTokenKeyValueState {
    state: BTreeMap<TokenStateKey, TokenStateValue>,
}

impl Tokens {
    pub fn hash(&self) -> PltBlockStateHash {
        if self.tokens.is_empty() {
            // For empty state, use a hash equal to the Haskell side. Else test suites in consensus must be updated
            // with new hashes. Also, eventually, our hashing must be compatible with Haskell PLT state anyway.
            PltBlockStateHash::from(
                <[u8; SHA256]>::try_from(
                    hex::decode("c423f9e91ee218b2b5303485dd87a3093a653ddb9bdb839d30aa1924de1dbf05")
                        .unwrap(),
                )
                .unwrap(),
            )
        } else {
            // todo do real implementation as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
            todo!()
        }
    }

    /// Get the [`TokenId`]s of all protocol-level tokens registered on the chain.
    pub fn plt_list(&self) -> impl Iterator<Item = TokenId> {
        self.tokens
            .iter()
            .map(|token| token.configuration.token_id.clone())
    }

    /// Create a new token with the given configuration. The initial state will be empty
    /// and the initial supply will be 0. Returns representation of the created token.
    ///
    /// # Arguments
    ///
    /// - `configuration` The configuration for the token.
    ///
    /// # Preconditions
    ///
    /// The caller must ensure the following conditions are true, and failing to do so results in
    /// undefined behavior.
    ///
    /// - The `token` of the given configuration MUST NOT already be in use by a protocol-level
    ///   token, i.e. `assert_eq!(s.get_token_index(configuration.token_id), None)`.
    pub fn create_token(&mut self, configuration: TokenConfiguration) -> TokenIndex {
        let token_index = TokenIndex(self.tokens.len() as u64);
        let token = Token {
            key_value_state: Default::default(),
            configuration,
            circulating_supply: Default::default(),
        };
        self.tokens.push(token);
        token_index
    }

    /// Get the configuration of a protocol-level token.
    ///
    /// # Arguments
    ///
    /// - `token` The token to get the config for.
    pub fn token_configuration(&self, token: TokenIndex) -> TokenConfiguration {
        let configuration = self.tokens[token.0 as usize].configuration.clone();
        TokenConfiguration {
            token_id: configuration.token_id,
            module_ref: configuration.module_ref,
            decimals: configuration.decimals,
        }
    }

    /// Get the token associated with a [`TokenId`] (if it exists).
    /// The token ID is case-insensitive when looking up tokens by token ID.
    ///
    /// # Arguments
    ///
    /// - `token_id` The token id to get the [`Self::Token`] of.
    pub fn token_by_id(&self, token_id: &TokenId) -> Result<TokenIndex, TokenNotFoundByIdError> {
        self.tokens
            .iter()
            .enumerate()
            .find_map(|(i, token)| {
                if token
                    .configuration
                    .token_id
                    .as_ref()
                    .eq_ignore_ascii_case(token_id.as_ref())
                {
                    Some(TokenIndex(i as u64))
                } else {
                    None
                }
            })
            .ok_or(TokenNotFoundByIdError(token_id.clone()))
    }

    /// Get the circulating supply of a protocol-level token.
    ///
    /// # Arguments
    ///
    /// - `token` The token to get the circulating supply.
    pub fn token_circulating_supply(&self, token: TokenIndex) -> RawTokenAmount {
        self.tokens[token.0 as usize].circulating_supply
    }

    /// Set the recorded total circulating supply for a protocol-level token.
    ///
    /// This should always be kept up-to-date with the total balance held in accounts.
    ///
    /// # Arguments
    ///
    /// - `token` The token.
    /// - `circulation_supply` The new total circulating supply for the token.
    pub fn set_token_circulating_supply(
        &mut self,
        token: TokenIndex,
        circulating_supply: RawTokenAmount,
    ) {
        self.tokens[token.0 as usize].circulating_supply = circulating_supply;
    }

    /// Convert a persistent token key-value state to a mutable one that can be updated by the scheduler.
    ///
    /// Updates to this state will only persist in the block state using [`BlockStateOperations::set_token_key_value_state`].
    ///
    /// # Arguments
    ///
    /// - `token` The token to get the token key-value state for.
    pub fn mutable_token_key_value_state(&self, token: TokenIndex) -> SimplisticTokenKeyValueState {
        self.tokens[token.0 as usize].key_value_state.clone()
    }

    /// Lookup the value for the given key in the given token key-value state. Returns `None` if
    /// no value exists for the given key.
    ///
    /// # Arguments
    ///
    /// - `token_module_map` The token module state to look up the value in.
    /// - `key` The token state key.
    pub fn lookup_token_state_value(
        &self,
        token_key_value_state: &SimplisticTokenKeyValueState,
        key: &TokenStateKey,
    ) -> Option<TokenStateValue> {
        token_key_value_state.state.get(key).cloned()
    }

    /// Update the value for the given key in the given token key-value state. If `None` is
    /// specified as value, the entry is removed.
    ///
    /// # Arguments
    ///
    /// - `token_module_map` The token module state to update the value in.
    /// - `key` The token state key.
    /// - `value` The value to set. If `None`, the entry with the given key is removed.
    pub fn update_token_state_value(
        &self,
        token_key_value_state: &mut SimplisticTokenKeyValueState,
        key: &TokenStateKey,
        value: Option<TokenStateValue>,
    ) {
        if let Some(value) = value {
            token_key_value_state.state.insert(key.clone(), value);
        } else {
            token_key_value_state.state.remove(key);
        }
    }

    /// Convert a mutable token key-value state to a persistent one and store it in the block state.
    ///
    /// To ensure this is future-proof, the mutable state should not be used after this call.
    ///
    /// # Arguments
    ///
    /// - `token` The token index to update.
    /// - `mutable_token_module_state` The mutated state to set as the current token state.
    pub fn set_token_key_value_state(
        &mut self,
        token: TokenIndex,
        token_key_value_state: SimplisticTokenKeyValueState,
    ) {
        self.tokens[token.0 as usize].key_value_state = token_key_value_state;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use concordium_base::protocol_level_tokens::TokenModuleRef;

    #[test]
    fn test_create_plt() {
        let mut tokens = Tokens::default();

        // Create token
        let configuration = TokenConfiguration {
            token_id: "token1".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 2,
        };
        let token = tokens.create_token(configuration.clone());

        // Read configuration
        let read_configuration = tokens.token_configuration(token);
        assert_eq!(read_configuration, configuration);
    }

    /// Test getting list of tokens.
    #[test]
    fn test_plt_list() {
        let mut tokens = Tokens::default();

        // Create token 1
        let token_id1: TokenId = "token1".parse().unwrap();
        let configuration = TokenConfiguration {
            token_id: token_id1.clone(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 2,
        };
        tokens.create_token(configuration.clone());

        // Create token 2
        let token_id2: TokenId = "token2".parse().unwrap();
        let configuration = TokenConfiguration {
            token_id: token_id2.clone(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 2,
        };
        tokens.create_token(configuration.clone());

        // Read PLT list
        let tokens: Vec<_> = tokens.plt_list().collect();
        assert_eq!(tokens, vec![token_id1, token_id2]);
    }

    /// Test getting token by id.
    #[test]
    fn test_token_by_id() {
        let mut tokens = Tokens::default();

        // Create token
        let token_id1: TokenId = "token1".parse().unwrap();
        let configuration = TokenConfiguration {
            token_id: token_id1.clone(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 2,
        };
        let token_index = tokens.create_token(configuration.clone());

        // Get token by id
        let token_index_by_id = tokens.token_by_id(&token_id1).expect("token should exist");
        assert_eq!(token_index_by_id, token_index);

        // Get token by non-canonical id
        let non_canonical_token_id1: TokenId = "TOKEN1".parse().unwrap();
        let token_index_by_id = tokens
            .token_by_id(&non_canonical_token_id1)
            .expect("token should exist");
        assert_eq!(token_index_by_id, token_index);

        // Get non-existing token by id
        let token_id2 = "token2".parse().unwrap();
        let err = tokens
            .token_by_id(&token_id2)
            .expect_err("token should not exist");
        assert_eq!(err.0, token_id2);
    }

    /// Test set and read circulating supply
    #[test]
    fn test_circulating_supply() {
        let mut tokens = Tokens::default();

        // Create token
        let configuration = TokenConfiguration {
            token_id: "token1".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 2,
        };
        let token = tokens.create_token(configuration.clone());

        // Assert initially 0
        let circulating_supply = tokens.token_circulating_supply(token);
        assert_eq!(circulating_supply, RawTokenAmount(0));

        // Set supply
        tokens.set_token_circulating_supply(token, RawTokenAmount(10));

        // Read supply
        let circulating_supply = tokens.token_circulating_supply(token);
        assert_eq!(circulating_supply, RawTokenAmount(10));
    }

    /// Test set and read circulating supply
    #[test]
    fn test_key_value_state() {
        let mut tokens = Tokens::default();

        // Create token
        let configuration = TokenConfiguration {
            token_id: "token1".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 2,
        };
        let token = tokens.create_token(configuration.clone());

        // Read key/value state
        let mut key_value_state = tokens.mutable_token_key_value_state(token);

        // Set entries
        tokens.update_token_state_value(&mut key_value_state, &vec![0, 1], Some(vec![0, 0]));
        tokens.update_token_state_value(&mut key_value_state, &vec![0, 2], Some(vec![1, 1]));

        // Set key/value state
        tokens.set_token_key_value_state(token, key_value_state);

        // Read key/value state again
        let mut key_value_state = tokens.mutable_token_key_value_state(token);

        // Read entries
        let value = tokens.lookup_token_state_value(&key_value_state, &vec![0, 1]);
        assert_eq!(value, Some(vec![0, 0]));
        let value = tokens.lookup_token_state_value(&key_value_state, &vec![0, 2]);
        assert_eq!(value, Some(vec![1, 1]));

        // Read non-existing entry
        let value = tokens.lookup_token_state_value(&key_value_state, &vec![0, 3]);
        assert_eq!(value, None);

        // Update entries
        tokens.update_token_state_value(&mut key_value_state, &vec![0, 1], Some(vec![2, 2]));
        tokens.update_token_state_value(&mut key_value_state, &vec![0, 2], None);

        // Set key/value state
        tokens.set_token_key_value_state(token, key_value_state);

        // Read key/value state again
        let key_value_state = tokens.mutable_token_key_value_state(token);

        // Read entries
        let value = tokens.lookup_token_state_value(&key_value_state, &vec![0, 1]);
        assert_eq!(value, Some(vec![2, 2]));
        let value = tokens.lookup_token_state_value(&key_value_state, &vec![0, 2]);
        assert_eq!(value, None);
    }
}
