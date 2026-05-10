//! Tests of the P11 block state.

use concordium_base::base::AccountIndex;
use concordium_base::protocol_level_tokens::{TokenAdminRole, TokenId, TokenModuleRef};
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::entity_test_stub;
use plt_block_state::entity::protocol_level_tokens::p9::TokenConfiguration;
use plt_block_state::entity::protocol_level_tokens::p11::Roles;

/// Test create a token in the block state and read its configuration.
#[test]
fn test_create_plt() {
    let context = entity_test_stub::new_context_no_external();
    let mut block_state = BlockStateP11::default();

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
        .token_p9
        .token_configuration(&context)
        .unwrap();
    assert_eq!(read_configuration, configuration);
}

/// Test getting list of tokens.
#[test]
fn test_plt_list() {
    let context = entity_test_stub::new_context_no_external();
    let mut block_state = BlockStateP11::default();

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
    let tokens = block_state.plt_list(&context).unwrap().to_vec();
    assert_eq!(tokens, vec![token_id1, token_id2]);
}

/// Test getting token by id.
#[test]
fn test_token_by_id() {
    let context = entity_test_stub::new_context_no_external();
    let mut block_state = BlockStateP11::default();

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
    assert_eq!(token_by_id.token_p9.token_index(), token_index);

    // Get token by non-canonical id
    let non_canonical_token_id1: TokenId = "TOKEN1".parse().unwrap();
    let token_index_by_id = block_state
        .token_by_id(&context, &non_canonical_token_id1)
        .unwrap()
        .expect("token should exist");
    assert_eq!(token_index_by_id.token_p9.token_index(), token_index);

    // Get non-existing token by id
    let token_id2 = "token2".parse().unwrap();
    let err = block_state
        .token_by_id(&context, &token_id2)
        .unwrap()
        .expect_err("token should not exist");
    assert_eq!(err.0, token_id2);
}

/// Test set and get token properties stored in the key-value state.
#[test]
fn test_token_properties() {
    let context = entity_test_stub::new_context_no_external();
    let mut block_state = BlockStateP11::default();

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
    let account_index1 = AccountIndex::from(1);
    assert_eq!(
        token.get_account_roles(&context, account_index1).unwrap(),
        Roles::none()
    );

    // Set values
    token
        .assign_account_roles(
            &context,
            account_index1,
            &[TokenAdminRole::Burn, TokenAdminRole::Mint],
        )
        .unwrap();

    // Update token
    block_state.update_token(&context, token).unwrap();

    // Read values
    let mut token = block_state.token_by_index(&context, token_index).unwrap();
    let mut expected_roles = Roles::none();
    expected_roles.assign(TokenAdminRole::Mint);
    expected_roles.assign(TokenAdminRole::Burn);
    assert_eq!(
        token.get_account_roles(&context, account_index1).unwrap(),
        expected_roles
    );
    // todo ar token authoriations
    // let expected_token_auths = TokenAuthorizations {
    //     update_admin_roles: None,
    //     mint: Some(TokenRoleAuthorizations {
    //         accounts: vec![],
    //     }),
    //     burn: None,
    //     update_allow_list: None,
    //     update_deny_list: None,
    //     pause: None,
    //     update_metadata: None,
    // };
    // assert_eq!(token.get_token_authorizations(&context, &block_state).unwrap(), expected_token_auths);

    // Update values
    token
        .revoke_account_roles(&context, account_index1, &[TokenAdminRole::Mint])
        .unwrap();

    // Update token
    block_state.update_token(&context, token).unwrap();

    // Read values
    let token = block_state.token_by_index(&context, token_index).unwrap();
    let mut expected_roles = Roles::none();
    expected_roles.assign(TokenAdminRole::Burn);
    assert_eq!(
        token.get_account_roles(&context, account_index1).unwrap(),
        expected_roles
    );
}
