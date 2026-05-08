//! Tests of the P9 block state.

use concordium_base::base::ProtocolVersion;
use concordium_base::protocol_level_tokens::{TokenId, TokenModuleRef};
use plt_block_state::block_state::hash::Hashable;
use plt_block_state::block_state_interface::{BlockStateOperations, BlockStateQuery};
use plt_block_state::entity::block_state::p9::BlockStateP9;
use plt_block_state::entity::entity_test_stub;
use plt_block_state::entity::protocol_level_tokens::p9::TokenConfiguration;
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// Test create a token in the block state.
#[test]
fn test_create_plt() {
    let context = entity_test_stub::new_context_no_external();
    let mut block_state = BlockStateP9::default();

    // Create token
    let configuration = TokenConfiguration {
        token_id: "token1".parse().unwrap(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    let token_index = block_state
        .create_token(&context, configuration.clone())
        .unwrap();

    // Read configuration
    let read_configuration = block_state
        .token_by_index(&context, token_index)
        .unwrap()
        .token_configuration(&context)
        .unwrap();
    assert_eq!(read_configuration, configuration);
}

/// Test getting list of tokens.
#[test]
fn test_plt_list() {
    let context = entity_test_stub::new_context_no_external();
    let mut block_state = BlockStateP9::default();

    // Create token 1
    let token_id1: TokenId = "token1".parse().unwrap();
    let configuration = TokenConfiguration {
        token_id: token_id1.clone(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    block_state
        .create_token(&context, configuration.clone())
        .unwrap();

    // Create token 2
    let token_id2: TokenId = "token2".parse().unwrap();
    let configuration = TokenConfiguration {
        token_id: token_id2.clone(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    block_state
        .create_token(&context, configuration.clone())
        .unwrap();

    // Read PLT list
    let tokens: Vec<_> = block_state
        .plt_list(&context)
        .map(|res| res.unwrap())
        .collect();
    assert_eq!(tokens, vec![token_id1, token_id2]);
}

/// Test getting token by id.
#[test]
fn test_token_by_id() {
    let context = entity_test_stub::new_context_no_external();
    let mut block_state = BlockStateP9::default();

    // Create token
    let token_id1: TokenId = "token1".parse().unwrap();
    let configuration = TokenConfiguration {
        token_id: token_id1.clone(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    let token_index = block_state
        .create_token(&context, configuration.clone())
        .unwrap();

    // Get token by id
    let token_by_id = block_state
        .token_by_id(&context, &token_id1)
        .unwrap()
        .expect("token should exist");
    assert_eq!(token_by_id.token_index(), token_index);

    // Get token by non-canonical id
    let non_canonical_token_id1: TokenId = "TOKEN1".parse().unwrap();
    let token_index_by_id = block_state
        .token_by_id(&context, &non_canonical_token_id1)
        .unwrap()
        .expect("token should exist");
    assert_eq!(token_index_by_id.token_index(), token_index);

    // Get non-existing token by id
    let token_id2 = "token2".parse().unwrap();
    let err = block_state
        .token_by_id(&context, &token_id2)
        .unwrap()
        .expect_err("token should not exist");
    assert_eq!(err.0, token_id2);
}

/// Test set and read circulating supply
#[test]
fn test_circulating_supply() {
    let context = entity_test_stub::new_context_no_external();
    let mut block_state = BlockStateP9::default();

    // Create token
    let configuration = TokenConfiguration {
        token_id: "token1".parse().unwrap(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    let token_index = block_state
        .create_token(&context, configuration.clone())
        .unwrap();
    let mut token = block_state.token_by_index(&context, token_index).unwrap();

    // Assert initially 0
    let circulating_supply = token.token_circulating_supply();
    assert_eq!(circulating_supply, RawTokenAmount(0));

    // Set supply
    token.set_token_circulating_supply(RawTokenAmount(10));

    // Update token
    block_state.update_token(&context, token).unwrap();

    // Read supply
    let token = block_state.token_by_index(&context, token_index).unwrap();
    assert_eq!(token.token_circulating_supply(), RawTokenAmount(10));
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
