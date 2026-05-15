//! Tests of the P11 block state.

use concordium_base::base::AccountIndex;
use concordium_base::common::types::TransactionTime;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::{TokenAdminRole, TokenId, TokenModuleRef};
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::entity_test_stub;
use plt_block_state::entity::protocol_level_tokens::p11::Roles;
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockConfiguration, LockControllerConfig, LockControllerSimpleV0,
};
use plt_block_state::persistent::protocol_level_tokens::p9::{TokenConfiguration, TokenIndex};

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

    // Read empty PLT list
    let tokens = block_state.plt_list(&context).unwrap().to_vec();
    assert_eq!(tokens, vec![]);

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

/// Test create a lock in the block state and read its configuration.
#[test]
fn test_create_lock() {
    let context = entity_test_stub::new_context_no_external();
    let mut block_state = BlockStateP11::default();

    // Create lock
    let configuration = LockConfiguration::new::<std::convert::Infallible>(
        [],
        TransactionTime::from(0u64),
        LockControllerConfig::SimpleV0(LockControllerSimpleV0 {
            grants: Vec::new(),
            tokens: Vec::new(),
            keep_alive: false,
            memo: None,
        }),
    )
    .unwrap();

    let lock_id = LockId {
        account_index: 1,
        sequence_number: 1,
        creation_order: 0,
    };

    block_state
        .create_lock(&context, lock_id.clone(), configuration.clone())
        .unwrap();

    // Read configuration
    let read_configuration = block_state
        .lock_by_id(&context, &lock_id)
        .unwrap()
        .unwrap()
        .lock_configuration(&context);
    assert_eq!(read_configuration, configuration);
}

/// Test getting lock by id.
#[test]
fn test_lock_by_id() {
    let context = entity_test_stub::new_context_no_external();
    let mut block_state = BlockStateP11::default();

    // Create lock
    let configuration = LockConfiguration::new::<std::convert::Infallible>(
        [],
        TransactionTime::from(0u64),
        LockControllerConfig::SimpleV0(LockControllerSimpleV0 {
            grants: Vec::new(),
            tokens: Vec::new(),
            keep_alive: false,
            memo: None,
        }),
    )
    .unwrap();

    let lock_id = LockId {
        account_index: 1,
        sequence_number: 1,
        creation_order: 0,
    };

    block_state
        .create_lock(&context, lock_id.clone(), configuration.clone())
        .unwrap();

    // Get lock by id
    let lock = block_state
        .lock_by_id(&context, &lock_id)
        .unwrap()
        .expect("lock should exist");
    assert_eq!(lock.lock_id(), &lock_id);

    // Get non-existing lock by id
    let non_existing_lock_id = LockId {
        account_index: 1,
        sequence_number: 2,
        creation_order: 0,
    };

    block_state
        .lock_by_id(&context, &non_existing_lock_id)
        .unwrap()
        .expect_err("lock should not exist");
}

/// Test set and get lock balance refs
#[test]
fn test_lock_balance_refs() {
    let context = entity_test_stub::new_context_no_external();
    let mut block_state = BlockStateP11::default();

    // Create lock
    let configuration = LockConfiguration::new::<std::convert::Infallible>(
        [],
        TransactionTime::from(0u64),
        LockControllerConfig::SimpleV0(LockControllerSimpleV0 {
            grants: Vec::new(),
            tokens: Vec::new(),
            keep_alive: false,
            memo: None,
        }),
    )
    .unwrap();

    let lock_id = LockId {
        account_index: 1,
        sequence_number: 1,
        creation_order: 0,
    };

    block_state
        .create_lock(&context, lock_id.clone(), configuration.clone())
        .unwrap();
    let mut lock = block_state
        .lock_by_id(&context, &lock_id)
        .unwrap()
        .expect("lock should exist");

    // Assert no initial balance refs
    assert_eq!(lock.lock_balance_refs(), vec![]);

    // Add balance refs
    lock.add_lock_balance_ref(AccountIndex::from(0), TokenIndex(0));
    lock.add_lock_balance_ref(AccountIndex::from(1), TokenIndex(1));

    // Update lock
    block_state.update_lock(&context, lock).unwrap();

    // Read balance refs
    let lock = block_state
        .lock_by_id(&context, &lock_id)
        .unwrap()
        .expect("lock should exist");
    assert_eq!(
        lock.lock_balance_refs(),
        vec![
            (AccountIndex::from(0), TokenIndex(0)),
            (AccountIndex::from(1), TokenIndex(1))
        ]
    );
}

/// Test getting list of locks. Mirrors `test_plt_list` for the lock side of the block state.
#[test]
fn test_lock_list() {
    let context = entity_test_stub::new_context_no_external();
    let mut block_state = BlockStateP11::default();

    // Read empty lock list
    let locks = block_state.lock_list(&context).unwrap();
    assert_eq!(locks, vec![]);

    // Create locks
    let configuration = LockConfiguration::new::<std::convert::Infallible>(
        [],
        TransactionTime::from(0u64),
        LockControllerConfig::SimpleV0(LockControllerSimpleV0 {
            grants: Vec::new(),
            tokens: Vec::new(),
            keep_alive: false,
            memo: None,
        }),
    )
    .unwrap();

    let lock_id_a = LockId {
        account_index: 1,
        sequence_number: 1,
        creation_order: 0,
    };
    let lock_id_b = LockId {
        account_index: 2,
        sequence_number: 7,
        creation_order: 0,
    };
    block_state
        .create_lock(&context, lock_id_a.clone(), configuration.clone())
        .unwrap();
    block_state
        .create_lock(&context, lock_id_b.clone(), configuration)
        .unwrap();

    // Read lock list
    let locks = block_state.lock_list(&context).unwrap();
    assert_eq!(locks, vec![lock_id_a, lock_id_b]);
}
