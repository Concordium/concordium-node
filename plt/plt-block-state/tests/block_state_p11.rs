//! Tests of the P11 block state.

use concordium_base::base::AccountIndex;
use concordium_base::common::types::TransactionTime;
use concordium_base::protocol_level_locks::{LockControllerSimpleV0Capability, LockId};
use concordium_base::protocol_level_tokens::{
    CborMemo, RawCbor, TokenAdminRole, TokenId, TokenModuleRef,
};
use concordium_base::transactions::Memo;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::entity_test_stub;
use plt_block_state::entity::protocol_level_tokens::p11::Roles;
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockConfiguration, LockControllerConfig, LockControllerSimpleV0, LockControllerSimpleV0Grant,
    LockRecipients,
};
use plt_block_state::persistent::protocol_level_tokens::p9::{TokenConfiguration, TokenIndex};
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// Test create a token in the block state and read its configuration.
#[test]
fn test_create_plt() {
    let context = entity_test_stub::new_no_external_context();
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
        .token_p9_base
        .token_configuration(&context)
        .unwrap();
    assert_eq!(read_configuration, configuration);
}

/// Test getting list of tokens.
#[test]
fn test_plt_list() {
    let context = entity_test_stub::new_no_external_context();
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
    let context = entity_test_stub::new_no_external_context();
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
    assert_eq!(token_by_id.token_p9_base.token_index(), token_index);

    // Get token by non-canonical id
    let non_canonical_token_id1: TokenId = "TOKEN1".parse().unwrap();
    let token_index_by_id = block_state
        .token_by_id(&context, &non_canonical_token_id1)
        .unwrap()
        .expect("token should exist");
    assert_eq!(token_index_by_id.token_p9_base.token_index(), token_index);

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
    let context = entity_test_stub::new_no_external_context();
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
    let lock_id = LockId {
        account_index: 7,
        sequence_number: 11,
        creation_order: 3,
    };
    assert_eq!(
        token.get_account_roles(&context, account_index1).unwrap(),
        Roles::none()
    );
    assert_eq!(token.all_roles(&context).unwrap(), vec![]);
    assert_eq!(
        token
            .get_locked_balance_for_account(&context, account_index1, &lock_id)
            .unwrap(),
        RawTokenAmount::from(0)
    );
    assert_eq!(
        token
            .get_locked_balances_for_account(&context, account_index1)
            .unwrap(),
        vec![]
    );

    // Set values
    token
        .assign_account_roles(
            &context,
            account_index1,
            &[TokenAdminRole::Burn, TokenAdminRole::Mint],
        )
        .unwrap();
    token
        .set_locked_balance_for_account(
            &context,
            account_index1,
            &lock_id,
            RawTokenAmount::from(100),
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
    assert_eq!(
        token.all_roles(&context).unwrap(),
        vec![(account_index1, expected_roles)]
    );
    assert_eq!(
        token
            .get_locked_balance_for_account(&context, account_index1, &lock_id)
            .unwrap(),
        RawTokenAmount::from(100)
    );
    assert_eq!(
        token
            .get_locked_balances_for_account(&context, account_index1)
            .unwrap(),
        vec![(lock_id.clone(), RawTokenAmount::from(100))]
    );

    // Update values
    token
        .revoke_account_roles(&context, account_index1, &[TokenAdminRole::Mint])
        .unwrap();
    token
        .set_locked_balance_for_account(&context, account_index1, &lock_id, RawTokenAmount::from(0))
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
    assert_eq!(
        token.all_roles(&context).unwrap(),
        vec![(account_index1, expected_roles)]
    );
    assert_eq!(
        token
            .get_locked_balance_for_account(&context, account_index1, &lock_id)
            .unwrap(),
        RawTokenAmount::from(0)
    );
    assert_eq!(
        token
            .get_locked_balances_for_account(&context, account_index1)
            .unwrap(),
        vec![]
    );
}

/// Test create a lock in the block state and read its configuration.
#[test]
fn test_create_lock() {
    let context = entity_test_stub::new_no_external_context();
    let mut block_state = BlockStateP11::default();

    // Create lock
    let lock_id = LockId {
        account_index: 1,
        sequence_number: 1,
        creation_order: 0,
    };
    let metadata = RawCbor::from(vec![0xa1]); // The node does not care what is in the metadata
    let configuration = LockConfiguration {
        recipients: LockRecipients::try_from(vec![AccountIndex::from(1), AccountIndex::from(2)])
            .unwrap(),
        expiry: TransactionTime::from(100u64),
        controller: LockControllerConfig::SimpleV0(
            LockControllerSimpleV0::new(
                vec![LockControllerSimpleV0Grant::new(
                    AccountIndex::from(1),
                    vec![
                        LockControllerSimpleV0Capability::Cancel,
                        LockControllerSimpleV0Capability::Fund,
                    ],
                )],
                vec!["tokenid1".parse().unwrap(), "tokenid2".parse().unwrap()],
                true,
                Some(CborMemo::Raw(Memo::try_from(vec![0, 1]).unwrap())),
            )
            .unwrap(),
        ),
        metadata: Some(metadata),
    };

    let mut locks = block_state.locks(&context).unwrap();
    locks
        .create(&context, &lock_id, configuration.clone())
        .unwrap();
    block_state.commit_locks(&context, locks);

    // Read configuration
    let read_configuration = block_state
        .lock_by_id(&context, &lock_id)
        .unwrap()
        .unwrap()
        .lock_configuration(&context)
        .unwrap()
        .into_owned();
    assert_eq!(read_configuration, configuration);

    let mut locks = block_state.locks(&context).unwrap();
    let duplicate = locks.create(&context, &lock_id, configuration.clone());
    assert!(duplicate.is_err(), "creating a duplicate lock ID must fail");
    assert_eq!(
        block_state
            .lock_by_id(&context, &lock_id)
            .unwrap()
            .unwrap()
            .lock_configuration(&context)
            .unwrap()
            .into_owned(),
        configuration
    );
}

/// Test getting lock by id.
#[test]
fn test_lock_by_id() {
    let context = entity_test_stub::new_no_external_context();
    let mut block_state = BlockStateP11::default();

    // Create lock
    let lock_id = LockId {
        account_index: 1,
        sequence_number: 1,
        creation_order: 0,
    };
    let configuration = LockConfiguration {
        recipients: LockRecipients::try_from(vec![]).unwrap(),
        expiry: TransactionTime::from(0u64),
        controller: LockControllerConfig::SimpleV0(
            LockControllerSimpleV0::new(Vec::new(), Vec::new(), false, None).unwrap(),
        ),
        metadata: None,
    };

    let mut locks = block_state.locks(&context).unwrap();
    locks.create(&context, &lock_id, configuration).unwrap();
    block_state.commit_locks(&context, locks);

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
    let context = entity_test_stub::new_no_external_context();
    let mut block_state = BlockStateP11::default();

    // Create lock
    let lock_id = LockId {
        account_index: 1,
        sequence_number: 1,
        creation_order: 0,
    };
    let configuration = LockConfiguration {
        recipients: LockRecipients::try_from(vec![]).unwrap(),
        expiry: TransactionTime::from(0u64),
        controller: LockControllerConfig::SimpleV0(
            LockControllerSimpleV0::new(Vec::new(), Vec::new(), false, None).unwrap(),
        ),
        metadata: None,
    };

    let mut locks = block_state.locks(&context).unwrap();
    locks.create(&context, &lock_id, configuration).unwrap();
    let mut lock = locks
        .by_id(&context, &lock_id)
        .unwrap()
        .expect("lock should exist");

    // Assert no initial balance refs
    assert_eq!(lock.lock_balance_refs(), vec![]);

    // Add balance refs
    lock.add_lock_balance_ref(AccountIndex::from(0), TokenIndex(0));
    lock.add_lock_balance_ref(AccountIndex::from(1), TokenIndex(1));

    // Update lock
    locks.update(&context, lock).unwrap();
    block_state.commit_locks(&context, locks);

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

/// Test creating a lock then deleting it.
#[test]
fn test_create_and_delete_lock() {
    let context = entity_test_stub::new_no_external_context();
    let mut block_state = BlockStateP11::default();

    // Create lock
    let lock_id = LockId {
        account_index: 1,
        sequence_number: 1,
        creation_order: 0,
    };
    let configuration = LockConfiguration {
        recipients: LockRecipients::try_from(vec![]).unwrap(),
        expiry: TransactionTime::from(0u64),
        controller: LockControllerConfig::SimpleV0(
            LockControllerSimpleV0::new(Vec::new(), Vec::new(), false, None).unwrap(),
        ),
        metadata: None,
    };

    let mut locks = block_state.locks(&context).unwrap();
    locks.create(&context, &lock_id, configuration).unwrap();
    block_state.commit_locks(&context, locks);

    // Verify lock exists
    block_state
        .lock_by_id(&context, &lock_id)
        .unwrap()
        .expect("lock should exist after creation");

    // Delete lock
    let mut locks = block_state.locks(&context).unwrap();
    let was_deleted = locks.delete(&context, &lock_id).unwrap();
    block_state.commit_locks(&context, locks);
    assert!(
        was_deleted,
        "delete_lock should return true for an existing lock"
    );

    // Verify lock no longer exists
    block_state
        .lock_by_id(&context, &lock_id)
        .unwrap()
        .expect_err("lock should not exist after deletion");

    // Deleting again should return false
    let mut locks = block_state.locks(&context).unwrap();
    let was_deleted_again = locks.delete(&context, &lock_id).unwrap();
    assert!(
        !was_deleted_again,
        "delete_lock should return false for a non-existing lock"
    );
}

/// Test getting list of locks. Mirrors `test_plt_list` for the lock side of the block state.
#[test]
fn test_lock_list() {
    let context = entity_test_stub::new_no_external_context();
    let mut block_state = BlockStateP11::default();

    // Read empty lock list
    let locks = block_state.lock_list(&context).unwrap();
    assert_eq!(locks, vec![]);

    // Create locks
    let lock_id_a = LockId {
        account_index: 1,
        sequence_number: 1,
        creation_order: 0,
    };
    let configuration_a = LockConfiguration {
        recipients: LockRecipients::try_from(vec![]).unwrap(),
        expiry: TransactionTime::from(0u64),
        controller: LockControllerConfig::SimpleV0(
            LockControllerSimpleV0::new(Vec::new(), Vec::new(), false, None).unwrap(),
        ),
        metadata: None,
    };

    let lock_id_b = LockId {
        account_index: 2,
        sequence_number: 7,
        creation_order: 0,
    };
    let configuration_b = LockConfiguration {
        recipients: LockRecipients::try_from(vec![]).unwrap(),
        expiry: TransactionTime::from(0u64),
        controller: LockControllerConfig::SimpleV0(
            LockControllerSimpleV0::new(Vec::new(), Vec::new(), false, None).unwrap(),
        ),
        metadata: None,
    };
    let mut locks = block_state.locks(&context).unwrap();
    locks.create(&context, &lock_id_a, configuration_a).unwrap();
    locks.create(&context, &lock_id_b, configuration_b).unwrap();
    assert!(locks.delete(&context, &lock_id_a).unwrap());
    block_state.commit_locks(&context, locks);

    // Read lock list and sort for a stable comparison (lock_list order is not guaranteed).
    let mut locks = block_state.lock_list(&context).unwrap();
    locks.sort();
    assert_eq!(locks, vec![lock_id_b]);
}
