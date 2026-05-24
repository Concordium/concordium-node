//! Tests of the P9 block state.

use concordium_base::base::AccountIndex;
use concordium_base::protocol_level_tokens::{MetadataUrl, TokenId, TokenModuleRef};
use plt_block_state::entity::block_state::p9::BlockStateP9;
use plt_block_state::entity::entity_test_stub;
use plt_block_state::persistent::protocol_level_tokens::p9::TokenConfiguration;
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// Test create a token in the block state and read its configuration.
#[test]
fn test_create_plt() {
    let context = entity_test_stub::new_no_external_context();
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
        .token_base
        .token_configuration(&context)
        .unwrap();
    assert_eq!(read_configuration, configuration);
}

/// Test getting list of tokens.
#[test]
fn test_plt_list() {
    let context = entity_test_stub::new_no_external_context();
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
    let context = entity_test_stub::new_no_external_context();
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
    assert_eq!(token_by_id.token_base.token_index(), token_index);

    // Get token by non-canonical id
    let non_canonical_token_id1: TokenId = "TOKEN1".parse().unwrap();
    let token_index_by_id = block_state
        .token_by_id(&context, &non_canonical_token_id1)
        .unwrap()
        .expect("token should exist");
    assert_eq!(token_index_by_id.token_base.token_index(), token_index);

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
    let context = entity_test_stub::new_no_external_context();
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
    let circulating_supply = token.token_base.token_circulating_supply();
    assert_eq!(circulating_supply, RawTokenAmount(0));

    // Set supply
    token
        .token_base
        .set_token_circulating_supply(RawTokenAmount(10));

    // Update token
    block_state.update_token(&context, token).unwrap();

    // Read supply
    let token = block_state.token_by_index(&context, token_index).unwrap();
    assert_eq!(
        token.token_base.token_circulating_supply(),
        RawTokenAmount(10)
    );
}

/// Test set and get token properties stored in the key-value state.
#[test]
fn test_token_properties() {
    let context = entity_test_stub::new_no_external_context();
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

    // Assert initial values
    assert!(!token.token_base.has_deny_list(&context));
    assert!(!token.token_base.has_allow_list(&context));
    assert!(!token.token_base.is_mintable(&context));
    assert!(!token.token_base.is_burnable(&context));
    assert!(!token.token_base.is_paused(&context));
    let account_index1 = AccountIndex::from(1);
    assert!(
        !token
            .token_base
            .get_allow_list_for(&context, account_index1)
    );
    assert!(!token.token_base.get_deny_list_for(&context, account_index1));

    // Set values
    token
        .token_base
        .set_token_circulating_supply(RawTokenAmount(10));
    token.token_base.set_deny_list_enabled(&context).unwrap();
    token.token_base.set_allow_list_enabled(&context).unwrap();
    token.token_base.set_mintable_enabled(&context).unwrap();
    token.token_base.set_burnable_enabled(&context).unwrap();
    token.token_base.set_paused(&context, true).unwrap();
    token.token_base.set_token_name(&context, "token1").unwrap();
    let gov_account_index = AccountIndex::from(10);
    token
        .token_base
        .set_governance_account(&context, gov_account_index)
        .unwrap();
    token
        .token_base
        .set_allow_list_for(&context, account_index1, true)
        .unwrap();
    token
        .token_base
        .set_deny_list_for(&context, account_index1, true)
        .unwrap();
    let metadata_url = MetadataUrl::from("http://test".to_string());
    token
        .token_base
        .set_metadata_url(&context, &metadata_url)
        .unwrap();

    // Update token
    block_state.update_token(&context, token).unwrap();

    // Read values
    let mut token = block_state.token_by_index(&context, token_index).unwrap();
    assert!(token.token_base.has_deny_list(&context));
    assert!(token.token_base.has_allow_list(&context));
    assert!(token.token_base.is_mintable(&context));
    assert!(token.token_base.is_burnable(&context));
    assert!(token.token_base.is_paused(&context));
    assert_eq!(token.token_base.get_token_name(&context).unwrap(), "token1");
    assert_eq!(
        token
            .token_base
            .get_governance_account_index(&context)
            .unwrap(),
        gov_account_index
    );
    assert!(
        token
            .token_base
            .get_allow_list_for(&context, account_index1)
    );
    assert!(token.token_base.get_deny_list_for(&context, account_index1));
    assert_eq!(
        token.token_base.get_metadata(&context).unwrap(),
        metadata_url
    );

    // Update values
    token.token_base.set_paused(&context, false).unwrap();
    token
        .token_base
        .set_allow_list_for(&context, account_index1, false)
        .unwrap();
    token
        .token_base
        .set_deny_list_for(&context, account_index1, false)
        .unwrap();

    // Update token
    block_state.update_token(&context, token).unwrap();

    // Read values
    let token = block_state.token_by_index(&context, token_index).unwrap();
    assert!(!token.token_base.is_paused(&context));
    assert!(
        !token
            .token_base
            .get_allow_list_for(&context, account_index1)
    );
    assert!(!token.token_base.get_deny_list_for(&context, account_index1));
}
