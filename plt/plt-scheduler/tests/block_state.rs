//! Tests of the block state [`PltBlockState`](plt_scheduler::block_state::PltBlockState).

use crate::block_state_stub::BlockStateWithExternalStateStubbed;
use concordium_base::protocol_level_tokens::{TokenId, TokenModuleRef};
use plt_scheduler::block_state::types::TokenConfiguration;
use plt_scheduler::block_state_interface::{BlockStateOperations, BlockStateQuery};
use plt_types::types::tokens::RawTokenAmount;

mod block_state_stub;

/// Test create a token in the block state.
#[test]
fn test_create_plt() {
    let mut block_state = BlockStateWithExternalStateStubbed::new();

    // Create token
    let configuration = TokenConfiguration {
        token_id: "token1".parse().unwrap(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    let token = block_state.state_mut().create_token(configuration.clone());

    // Read configuration
    let read_configuration = block_state.state().token_configuration(&token);
    assert_eq!(read_configuration, configuration);
}

/// Test getting list of tokens.
#[test]
fn test_plt_list() {
    let mut block_state = BlockStateWithExternalStateStubbed::new();

    // Create token 1
    let token_id1: TokenId = "token1".parse().unwrap();
    let configuration = TokenConfiguration {
        token_id: token_id1.clone(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    block_state.state_mut().create_token(configuration.clone());

    // Create token 2
    let token_id2: TokenId = "token2".parse().unwrap();
    let configuration = TokenConfiguration {
        token_id: token_id2.clone(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    block_state.state_mut().create_token(configuration.clone());

    // Read PLT list
    let tokens: Vec<_> = block_state.state().plt_list().collect();
    assert_eq!(tokens, vec![token_id1, token_id2]);
}

/// Test getting token by id.
#[test]
fn test_token_by_id() {
    let mut block_state = BlockStateWithExternalStateStubbed::new();

    // Create token
    let token_id1: TokenId = "token1".parse().unwrap();
    let configuration = TokenConfiguration {
        token_id: token_id1.clone(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    let token_index = block_state.state_mut().create_token(configuration.clone());

    // Get token by id
    let token_index_by_id = block_state
        .state()
        .token_by_id(&token_id1)
        .expect("token should exist");
    assert_eq!(token_index_by_id, token_index);

    // Get token by non-canonical id
    let non_canonical_token_id1: TokenId = "TOKEN1".parse().unwrap();
    let token_index_by_id = block_state
        .state()
        .token_by_id(&non_canonical_token_id1)
        .expect("token should exist");
    assert_eq!(token_index_by_id, token_index);

    // Get non-existing token by id
    let token_id2 = "token2".parse().unwrap();
    let err = block_state
        .state()
        .token_by_id(&token_id2)
        .expect_err("token should not exist");
    assert_eq!(err.0, token_id2);
}

/// Test set and read circulating supply
#[test]
fn test_circulating_supply() {
    let mut block_state = BlockStateWithExternalStateStubbed::new();

    // Create token
    let configuration = TokenConfiguration {
        token_id: "token1".parse().unwrap(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    let token = block_state.state_mut().create_token(configuration.clone());

    // Assert initially 0
    let circulating_supply = block_state.state().token_circulating_supply(&token);
    assert_eq!(circulating_supply, RawTokenAmount(0));

    // Set supply
    block_state
        .state_mut()
        .set_token_circulating_supply(&token, RawTokenAmount(10));

    // Read supply
    let circulating_supply = block_state.state().token_circulating_supply(&token);
    assert_eq!(circulating_supply, RawTokenAmount(10));
}

/// Test set and read circulating supply
#[test]
fn test_key_value_state() {
    let mut block_state = BlockStateWithExternalStateStubbed::new();

    // Create token
    let configuration = TokenConfiguration {
        token_id: "token1".parse().unwrap(),
        module_ref: TokenModuleRef::from([5; 32]),
        decimals: 2,
    };
    let token = block_state.state_mut().create_token(configuration.clone());

    // Read key/value state
    let mut key_value_state = block_state.state().mutable_token_key_value_state(&token);

    // Set entries
    block_state.state().update_token_state_value(
        &mut key_value_state,
        &vec![0, 1],
        Some(vec![0, 0]),
    );
    block_state.state().update_token_state_value(
        &mut key_value_state,
        &vec![0, 2],
        Some(vec![1, 1]),
    );

    // Set key/value state
    block_state
        .state_mut()
        .set_token_key_value_state(&token, key_value_state);

    // Read key/value state again
    let mut key_value_state = block_state.state().mutable_token_key_value_state(&token);

    // Read entries
    let value = block_state
        .state()
        .lookup_token_state_value(&key_value_state, &vec![0, 1]);
    assert_eq!(value, Some(vec![0, 0]));
    let value = block_state
        .state()
        .lookup_token_state_value(&key_value_state, &vec![0, 2]);
    assert_eq!(value, Some(vec![1, 1]));

    // Read non-existing entry
    let value = block_state
        .state()
        .lookup_token_state_value(&key_value_state, &vec![0, 3]);
    assert_eq!(value, None);

    // Update entries
    block_state.state().update_token_state_value(
        &mut key_value_state,
        &vec![0, 1],
        Some(vec![2, 2]),
    );
    block_state
        .state()
        .update_token_state_value(&mut key_value_state, &vec![0, 2], None);

    // Set key/value state
    block_state
        .state_mut()
        .set_token_key_value_state(&token, key_value_state);

    // Read key/value state again
    let key_value_state = block_state.state().mutable_token_key_value_state(&token);

    // Read entries
    let value = block_state
        .state()
        .lookup_token_state_value(&key_value_state, &vec![0, 1]);
    assert_eq!(value, Some(vec![2, 2]));
    let value = block_state
        .state()
        .lookup_token_state_value(&key_value_state, &vec![0, 2]);
    assert_eq!(value, None);
}
