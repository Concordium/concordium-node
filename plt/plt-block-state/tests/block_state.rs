//! Tests of the block state [`PltBlockState`](plt_scheduler::block_state::PltBlockState).

use concordium_base::base::ProtocolVersion;
use concordium_base::protocol_level_tokens::{TokenId, TokenModuleRef};
use plt_block_state::block_state::hash::Hashable;
use plt_block_state::block_state::types::protocol_level_tokens::{
    TokenConfiguration, TokenStateKey, TokenStateValue,
};
use plt_block_state::block_state::{BlockState, blob_store};
use plt_block_state::block_state_interface::{BlockStateOperations, BlockStateQuery};
use plt_scheduler_types::types::tokens::RawTokenAmount;

mod block_state_no_external;

/// Test create a token in the block state.
#[test]
fn test_create_plt() {
    let mut block_state = block_state_no_external::new_mutable_block_state(ProtocolVersion::P11);

    // Create token
    let configuration = TokenConfiguration {
        token_id: "token1".parse().unwrap(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    let token = block_state.create_token(configuration.clone());

    // Read configuration
    let read_configuration = block_state.token_configuration(&token);
    assert_eq!(read_configuration, configuration);
}

/// Test getting list of tokens.
#[test]
fn test_plt_list() {
    let mut block_state = block_state_no_external::new_mutable_block_state(ProtocolVersion::P11);

    // Create token 1
    let token_id1: TokenId = "token1".parse().unwrap();
    let configuration = TokenConfiguration {
        token_id: token_id1.clone(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    block_state.create_token(configuration.clone());

    // Create token 2
    let token_id2: TokenId = "token2".parse().unwrap();
    let configuration = TokenConfiguration {
        token_id: token_id2.clone(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    block_state.create_token(configuration.clone());

    // Read PLT list
    let tokens: Vec<_> = block_state.plt_list().collect();
    assert_eq!(tokens, vec![token_id1, token_id2]);
}

/// Test getting token by id.
#[test]
fn test_token_by_id() {
    let mut block_state = block_state_no_external::new_mutable_block_state(ProtocolVersion::P11);

    // Create token
    let token_id1: TokenId = "token1".parse().unwrap();
    let configuration = TokenConfiguration {
        token_id: token_id1.clone(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    let token_index = block_state.create_token(configuration.clone());

    // Get token by id
    let token_index_by_id = block_state
        .token_by_id(&token_id1)
        .expect("token should exist");
    assert_eq!(token_index_by_id, token_index);

    // Get token by non-canonical id
    let non_canonical_token_id1: TokenId = "TOKEN1".parse().unwrap();
    let token_index_by_id = block_state
        .token_by_id(&non_canonical_token_id1)
        .expect("token should exist");
    assert_eq!(token_index_by_id, token_index);

    // Get non-existing token by id
    let token_id2 = "token2".parse().unwrap();
    let err = block_state
        .token_by_id(&token_id2)
        .expect_err("token should not exist");
    assert_eq!(err.0, token_id2);
}

/// Test set and read circulating supply
#[test]
fn test_circulating_supply() {
    let mut block_state = block_state_no_external::new_mutable_block_state(ProtocolVersion::P11);

    // Create token
    let configuration = TokenConfiguration {
        token_id: "token1".parse().unwrap(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    let token = block_state.create_token(configuration.clone());

    // Assert initially 0
    let circulating_supply = block_state.token_circulating_supply(&token);
    assert_eq!(circulating_supply, RawTokenAmount(0));

    // Set supply
    block_state.set_token_circulating_supply(&token, RawTokenAmount(10));

    // Read supply
    let circulating_supply = block_state.token_circulating_supply(&token);
    assert_eq!(circulating_supply, RawTokenAmount(10));
}

/// Test mutate and set token key-value state. Including
/// updating, deleting and reading entries.
#[test]
fn test_key_value_state() {
    let mut block_state = block_state_no_external::new_mutable_block_state(ProtocolVersion::P11);

    // Create token
    let configuration = TokenConfiguration {
        token_id: "token1".parse().unwrap(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    let token = block_state.create_token(configuration.clone());

    // Thaw key-value state
    let mut key_value_state = block_state.mutable_token_key_value_state(&token);

    // Set entries
    block_state.update_token_state_value(
        &mut key_value_state,
        &TokenStateKey(vec![0, 1]),
        Some(TokenStateValue(vec![0, 0])),
    );
    block_state.update_token_state_value(
        &mut key_value_state,
        &TokenStateKey(vec![0, 2]),
        Some(TokenStateValue(vec![1, 1])),
    );

    // Set key-value state
    block_state.set_token_key_value_state(&token, key_value_state);

    // Thaw key-value state
    let mut key_value_state = block_state.mutable_token_key_value_state(&token);

    // Read entries
    let value = block_state.lookup_token_state_value(&key_value_state, &TokenStateKey(vec![0, 1]));
    assert_eq!(value, Some(TokenStateValue(vec![0, 0])));
    let value = block_state.lookup_token_state_value(&key_value_state, &TokenStateKey(vec![0, 2]));
    assert_eq!(value, Some(TokenStateValue(vec![1, 1])));

    // Read non-existing entry
    let value = block_state.lookup_token_state_value(&key_value_state, &TokenStateKey(vec![0, 3]));
    assert_eq!(value, None);

    // Update and delete entries
    block_state.update_token_state_value(
        &mut key_value_state,
        &TokenStateKey(vec![0, 1]),
        Some(TokenStateValue(vec![2, 2])),
    );
    block_state.update_token_state_value(&mut key_value_state, &TokenStateKey(vec![0, 2]), None);

    // Set key-value state
    block_state.set_token_key_value_state(&token, key_value_state);

    // Thaw key-value state
    let key_value_state = block_state.mutable_token_key_value_state(&token);

    // Read entries
    let value = block_state.lookup_token_state_value(&key_value_state, &TokenStateKey(vec![0, 1]));
    assert_eq!(value, Some(TokenStateValue(vec![2, 2])));
    let value = block_state.lookup_token_state_value(&key_value_state, &TokenStateKey(vec![0, 2]));
    assert_eq!(value, None);
}

/// Iterate values in token key-value state by prefix.
#[test]
fn test_key_value_state_prefix_iter() {
    let mut block_state = block_state_no_external::new_mutable_block_state(ProtocolVersion::P11);

    // Create token
    let configuration = TokenConfiguration {
        token_id: "token1".parse().unwrap(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    let token = block_state.create_token(configuration.clone());

    // Thaw key-value state
    let mut key_value_state = block_state.mutable_token_key_value_state(&token);

    // Set entries
    block_state.update_token_state_value(
        &mut key_value_state,
        &TokenStateKey(vec![0, 1]),
        Some(TokenStateValue(vec![0, 0])),
    );
    block_state.update_token_state_value(
        &mut key_value_state,
        &TokenStateKey(vec![1, 1]),
        Some(TokenStateValue(vec![1, 1])),
    );
    block_state.update_token_state_value(
        &mut key_value_state,
        &TokenStateKey(vec![1, 2]),
        Some(TokenStateValue(vec![2, 2])),
    );
    block_state.update_token_state_value(
        &mut key_value_state,
        &TokenStateKey(vec![2, 1]),
        Some(TokenStateValue(vec![3, 3])),
    );

    // Set key-value state
    block_state.set_token_key_value_state(&token, key_value_state);

    // Thaw key-value state
    let key_value_state = block_state.mutable_token_key_value_state(&token);

    // Iterate entries
    let entries: Vec<_> = block_state
        .iter_token_state_prefix(&key_value_state, &TokenStateKey(vec![1]))
        .collect();
    assert_eq!(
        entries,
        vec![
            (TokenStateKey(vec![1, 1]), TokenStateValue(vec![1, 1])),
            (TokenStateKey(vec![1, 2]), TokenStateValue(vec![2, 2]))
        ]
    );
    let entries: Vec<_> = block_state
        .iter_token_state_prefix(&key_value_state, &TokenStateKey(vec![3]))
        .collect();
    assert_eq!(entries, vec![]);
}

/// Store state with PLTs to blob store and load it again.
///
/// todo extend this test with a blob store fixture that matches the haskell side part of https://linear.app/concordium/issue/PSR-85/implement-test-of-storage-and-hashing
#[test]
fn test_store_and_load_plts() {
    let mut block_state = block_state_no_external::new_mutable_block_state(ProtocolVersion::P11);

    // Create tokens
    let configuration1 = TokenConfiguration {
        token_id: "token1".parse().unwrap(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    let token1 = block_state.create_token(configuration1.clone());
    block_state.set_token_circulating_supply(&token1, RawTokenAmount(100));
    let mut key_value_state1 = block_state.mutable_token_key_value_state(&token1);
    block_state.update_token_state_value(
        &mut key_value_state1,
        &TokenStateKey(vec![0, 1]),
        Some(TokenStateValue(vec![0, 0])),
    );
    block_state.update_token_state_value(
        &mut key_value_state1,
        &TokenStateKey(vec![0, 2]),
        Some(TokenStateValue(vec![1, 1])),
    );
    block_state.set_token_key_value_state(&token1, key_value_state1);
    let configuration2 = TokenConfiguration {
        token_id: "token2".parse().unwrap(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 4,
    };
    let token2 = block_state.create_token(configuration2.clone());

    // Store block state
    let blob_ref = blob_store::store_to_store(
        &mut block_state.blob_store_load,
        block_state.internal_block_state.into_immutable(),
    );

    // Load block state
    let immutable_state =
        blob_store::load_from_store::<BlockState>(&block_state.blob_store_load, blob_ref)
            .expect("load block state");
    let block_state = block_state_no_external::with_block_state(
        ProtocolVersion::P11,
        block_state.blob_store_load,
        &immutable_state,
    );

    // Assert loaded state
    assert_eq!(block_state.plt_list().len(), 2);
    assert_eq!(
        block_state.token_circulating_supply(&token1),
        RawTokenAmount(100)
    );
    assert_eq!(block_state.token_configuration(&token1), configuration1);
    let key_value_state1 = block_state.mutable_token_key_value_state(&token1);
    let value = block_state.lookup_token_state_value(&key_value_state1, &TokenStateKey(vec![0, 1]));
    assert_eq!(value, Some(TokenStateValue(vec![0, 0])));
    let value = block_state.lookup_token_state_value(&key_value_state1, &TokenStateKey(vec![0, 2]));
    assert_eq!(value, Some(TokenStateValue(vec![1, 1])));
    assert_eq!(
        block_state.token_circulating_supply(&token2),
        RawTokenAmount(0)
    );
    assert_eq!(block_state.token_configuration(&token2), configuration2);
}

/// Assert hash block state with PLTs matches a fixed/snapshot hash. The hash
/// must remain stable.
///
/// todo extend this test on the haskell side as part of https://linear.app/concordium/issue/PSR-85/implement-test-of-storage-and-hashing
#[test]
fn snapshot_test_hash_plts() {
    let mut block_state = block_state_no_external::new_mutable_block_state(ProtocolVersion::P11);

    // Create tokens
    let configuration1 = TokenConfiguration {
        token_id: "token1".parse().unwrap(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    let token1 = block_state.create_token(configuration1.clone());
    block_state.set_token_circulating_supply(&token1, RawTokenAmount(100));
    let mut key_value_state1 = block_state.mutable_token_key_value_state(&token1);
    block_state.update_token_state_value(
        &mut key_value_state1,
        &TokenStateKey(vec![0, 1]),
        Some(TokenStateValue(vec![0, 0])),
    );
    block_state.update_token_state_value(
        &mut key_value_state1,
        &TokenStateKey(vec![0, 2]),
        Some(TokenStateValue(vec![1, 1])),
    );
    block_state.set_token_key_value_state(&token1, key_value_state1);
    let configuration2 = TokenConfiguration {
        token_id: "token2".parse().unwrap(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 4,
    };
    let _token2 = block_state.create_token(configuration2.clone());

    // Assert hash
    let immutable_state = block_state.internal_block_state.into_immutable();
    let hash = immutable_state
        .hash(&block_state.blob_store_load)
        .expect("hash");
    assert_eq!(
        format!("{}", hash),
        "231140c20455e597dd5e9a6f09f0bdb718d68e400a241a10a98cda35e77baa05"
    );
}
