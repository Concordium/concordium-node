//! Tests for cancelling a PLT lock.

use crate::utils::entity_traits::scheduler::SchedulerOperations;
use crate::utils::{BlockStateLatest, TokenInitTestParams};
use assert_matches::assert_matches;
use concordium_base::protocol_level_tokens::CborMemo;
use concordium_base::protocol_level_tokens::meta_operations::lock_fund;
use concordium_base::{
    base::Energy,
    common::cbor,
    protocol_level_locks::{LockControllerSimpleV0Capability, LockId},
    protocol_level_tokens::{
        CborHolderAccount, RawCbor, TokenId, TokenListUpdateDetails, TokenOperation,
        meta_operations::{MetaUpdatePayload, lock_cancel},
    },
    transactions::Payload,
};
use plt_block_state::entity::accounts::Accounts;
use plt_block_state::entity::block_state::LockNotFoundByIdError;
use plt_block_state::{
    entity::entity_test_stub, persistent::protocol_level_locks::p11::LockControllerSimpleV0Grant,
};
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;
use plt_scheduler_types::types::tokens::TokenHolder;
use plt_scheduler_types::types::{
    events::{BlockItemEvent, LockDestroyEvent},
    execution::TransactionOutcome,
    tokens::{RawTokenAmount, TokenAmount},
};

mod utils;

/// Test cancelling a lock by an authorized canceller before the lock's expiry time.
#[test]
fn test_cancel_by_canceller() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let account_index_1 = context.external.create_account().account_index();
    let account_index_2 = context.external.create_account().account_index();

    let plt_x: TokenId = "pltX".parse().unwrap();
    let parameters = TokenInitTestParams::default().mintable().burnable();
    let (_gov_acct, _token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        plt_x.clone(),
        parameters,
        2,
        Some(RawTokenAmount::from(10000)),
    );

    let lock_id = LockId {
        account_index: account_index_1.into(),
        sequence_number: 2,
        creation_order: 0,
    };
    let lock_config = utils::CreateLockSimpleConfig {
        recipients: vec![account_index_1],
        grants: vec![LockControllerSimpleV0Grant {
            account: account_index_2,
            roles: vec![LockControllerSimpleV0Capability::Cancel],
        }],
        tokens: vec![plt_x.clone()],
        expiry: 1000,
        keep_alive: false,
    };
    utils::create_lock(&mut context, &mut block_state, &lock_id, lock_config);

    let transaction_context = plt_scheduler::TransactionContext {
        energy_limit: Energy::from(u64::MAX),
        sender_account_address: context.external.account_canonical_address(account_index_2),
        transaction_sequence_number: 1.into(),
        block_timestamp: 0.into(),
    };
    let payload = Payload::MetaUpdate {
        payload: MetaUpdatePayload {
            operations: RawCbor::from(cbor::cbor_encode(&vec![lock_cancel(lock_id.clone(), None)])),
        },
    };
    let summary = block_state
        .execute_transaction(&mut context, transaction_context, account_index_2, payload)
        .unwrap();
    assert_matches!(summary.outcome, TransactionOutcome::Success(events) => {
        assert_eq!(events.len(), 1);
        assert_matches!(&events[0], BlockItemEvent::LockDestroyed(LockDestroyEvent{lock_id: event_lock_id}) => {
            assert_eq!(event_lock_id, &lock_id);
        })
    });
}

/// Test cancelling a lock by an unauthorized account before the lock's expiry time.
#[test]
fn test_cancel_unauthorized() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let account_index_1 = context.external.create_account().account_index();
    let account_index_2 = context.external.create_account().account_index();

    let plt_x: TokenId = "pltX".parse().unwrap();
    let parameters = TokenInitTestParams::default().mintable().burnable();
    let (_gov_acct, _token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        plt_x.clone(),
        parameters,
        2,
        Some(RawTokenAmount::from(10000)),
    );

    let lock_id = LockId {
        account_index: account_index_1.into(),
        sequence_number: 2,
        creation_order: 0,
    };
    let lock_config = utils::CreateLockSimpleConfig {
        recipients: vec![account_index_1],
        grants: vec![LockControllerSimpleV0Grant {
            account: account_index_1,
            roles: vec![LockControllerSimpleV0Capability::Cancel],
        }],
        tokens: vec![plt_x.clone()],
        expiry: 1000,
        keep_alive: false,
    };
    utils::create_lock(&mut context, &mut block_state, &lock_id, lock_config);

    let sender_addr = context.external.account_canonical_address(account_index_2);
    let transaction_context = plt_scheduler::TransactionContext {
        energy_limit: Energy::from(u64::MAX),
        sender_account_address: sender_addr,
        transaction_sequence_number: 1.into(),
        block_timestamp: 0.into(),
    };
    let payload = Payload::MetaUpdate {
        payload: MetaUpdatePayload {
            operations: RawCbor::from(cbor::cbor_encode(&vec![lock_cancel(lock_id.clone(), None)])),
        },
    };
    let summary = block_state
        .execute_transaction(&mut context, transaction_context, account_index_2, payload)
        .unwrap();
    assert_matches!(summary.outcome, TransactionOutcome::Rejected(reason) => {
        assert_eq!(reason, TransactionRejectReason::LockCancelNotAuthorized(lock_id.clone(), sender_addr));
    });
}

/// Test cancelling a lock after the lock's expiry time, by an account with no
/// cancel capability.
#[test]
fn test_cancel_after_expiry() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let account_index_1 = context.external.create_account().account_index();
    let account_index_2 = context.external.create_account().account_index();

    let plt_x: TokenId = "pltX".parse().unwrap();
    let parameters = TokenInitTestParams::default().mintable().burnable();
    let (_gov_acct, _token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        plt_x.clone(),
        parameters,
        2,
        Some(RawTokenAmount::from(10000)),
    );

    let lock_id = LockId {
        account_index: account_index_1.into(),
        sequence_number: 2,
        creation_order: 0,
    };
    let lock_config = utils::CreateLockSimpleConfig {
        recipients: vec![account_index_1],
        grants: vec![LockControllerSimpleV0Grant {
            account: account_index_2,
            roles: vec![LockControllerSimpleV0Capability::Cancel],
        }],
        tokens: vec![plt_x.clone()],
        expiry: 1000,
        keep_alive: false,
    };
    utils::create_lock(&mut context, &mut block_state, &lock_id, lock_config);

    let transaction_context = plt_scheduler::TransactionContext {
        energy_limit: Energy::from(u64::MAX),
        sender_account_address: context.external.account_canonical_address(account_index_1),
        transaction_sequence_number: 1.into(),
        block_timestamp: 1000001.into(),
    };
    let payload = Payload::MetaUpdate {
        payload: MetaUpdatePayload {
            operations: RawCbor::from(cbor::cbor_encode(&vec![lock_cancel(lock_id.clone(), None)])),
        },
    };
    let summary = block_state
        .execute_transaction(&mut context, transaction_context, account_index_1, payload)
        .unwrap();
    assert_matches!(summary.outcome, TransactionOutcome::Success(events) => {
        assert_eq!(events.len(), 1);
        assert_matches!(&events[0], BlockItemEvent::LockDestroyed(LockDestroyEvent{lock_id: event_lock_id}) => {
            assert_eq!(event_lock_id, &lock_id);
        })
    });
}

/// Test cancelling a lock with balances.
#[test]
fn test_cancel_with_balances() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let account_index_1 = context.external.create_account().account_index();
    let account_index_2 = context.external.create_account().account_index();

    let plt_x: TokenId = "pltX".parse().unwrap();
    let parameters = TokenInitTestParams::default().mintable().burnable();
    let (plt_x_gov_acct, _token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        plt_x.clone(),
        parameters,
        2,
        Some(RawTokenAmount::from(10000)),
    );
    let plt_x_gov_acct_address = context
        .account_by_index(plt_x_gov_acct.account_index())
        .unwrap()
        .canonical_account_address;
    let plt_y: TokenId = "pltY".parse().unwrap();
    let (plt_y_gov_acct, _token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        plt_y.clone(),
        TokenInitTestParams::default(),
        6,
        Some(RawTokenAmount::from(10000000)),
    );
    let plt_y_gov_acct_address = context
        .account_by_index(plt_y_gov_acct.account_index())
        .unwrap()
        .canonical_account_address;

    let lock_id = LockId {
        account_index: account_index_1.into(),
        sequence_number: 2,
        creation_order: 0,
    };
    let lock_config = utils::CreateLockSimpleConfig {
        recipients: vec![account_index_1],
        grants: vec![
            LockControllerSimpleV0Grant {
                account: account_index_2,
                roles: vec![LockControllerSimpleV0Capability::Cancel],
            },
            LockControllerSimpleV0Grant {
                account: plt_x_gov_acct.account_index(),
                roles: vec![LockControllerSimpleV0Capability::Fund],
            },
            LockControllerSimpleV0Grant {
                account: plt_y_gov_acct.account_index(),
                roles: vec![
                    LockControllerSimpleV0Capability::Fund,
                    LockControllerSimpleV0Capability::Send,
                ],
            },
        ],
        tokens: vec![plt_x.clone()],
        expiry: 1000,
        keep_alive: false,
    };
    utils::create_lock(&mut context, &mut block_state, &lock_id, lock_config);
    utils::lock_balance(
        &mut context,
        &mut block_state,
        &lock_id,
        plt_x_gov_acct.account_index(),
        &plt_x.clone(),
        RawTokenAmount::from(500),
    );
    utils::lock_balance(
        &mut context,
        &mut block_state,
        &lock_id,
        plt_y_gov_acct.account_index(),
        &plt_y.clone(),
        RawTokenAmount::from(1000),
    );

    let transaction_context = plt_scheduler::TransactionContext {
        energy_limit: Energy::from(u64::MAX),
        sender_account_address: context.external.account_canonical_address(account_index_2),
        transaction_sequence_number: 1.into(),
        block_timestamp: 0.into(),
    };
    let memo = CborMemo::Raw(vec![1u8, 2, 3].try_into().unwrap());
    let payload = Payload::MetaUpdate {
        payload: MetaUpdatePayload {
            operations: RawCbor::from(cbor::cbor_encode(&vec![lock_cancel(
                lock_id.clone(),
                Some(memo.clone()),
            )])),
        },
    };
    let summary = block_state
        .execute_transaction(&mut context, transaction_context, account_index_2, payload)
        .unwrap();
    assert_matches!(summary.outcome, TransactionOutcome::Success(events) => {
        assert_eq!(events.len(), 3);
        assert_matches!(&events[0], BlockItemEvent::TokenTransfer(transfer) => {
            assert_eq!(transfer.token_id, plt_x);
            assert_eq!(transfer.amount, TokenAmount::from_raw(500, 2));
            assert_eq!(transfer.from, TokenHolder::Account(plt_x_gov_acct_address));
            assert_eq!(transfer.to, TokenHolder::Account(plt_x_gov_acct_address));
            assert_eq!(transfer.from_lock.as_ref(), Some(&lock_id));
            assert_eq!(transfer.to_lock, None);
            assert_eq!(transfer.memo, Some(memo.clone().into()));
        });
        assert_matches!(&events[1], BlockItemEvent::TokenTransfer(transfer) => {
            assert_eq!(transfer.token_id, plt_y);
            assert_eq!(transfer.amount, TokenAmount::from_raw(1000, 6));
            assert_eq!(transfer.from, TokenHolder::Account(plt_y_gov_acct_address));
            assert_eq!(transfer.to, TokenHolder::Account(plt_y_gov_acct_address));
            assert_eq!(transfer.from_lock.as_ref(), Some(&lock_id));
            assert_eq!(transfer.to_lock, None);
            assert_eq!(transfer.memo, Some(memo.into()));
        });
        assert_matches!(&events[2], BlockItemEvent::LockDestroyed(LockDestroyEvent{lock_id: event_lock_id}) => {
            assert_eq!(event_lock_id, &lock_id);
        })
    });
    assert_matches!(block_state.lock_by_id(&context, &lock_id), Ok(Err(LockNotFoundByIdError(absent_id))) => {
        assert_eq!(absent_id, lock_id);
    });
}

/// Test cancelling a non-existent lock.
#[test]
fn test_cancel_nonexistent() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let account_index_1 = context.external.create_account().account_index();

    let transaction_context = plt_scheduler::TransactionContext {
        energy_limit: Energy::from(u64::MAX),
        sender_account_address: context.external.account_canonical_address(account_index_1),
        transaction_sequence_number: 1.into(),
        block_timestamp: 0.into(),
    };
    let memo = CborMemo::Raw(vec![1u8, 2, 3].try_into().unwrap());
    let lock_id = LockId {
        account_index: account_index_1.into(),
        sequence_number: 999,
        creation_order: 0,
    };
    let payload = Payload::MetaUpdate {
        payload: MetaUpdatePayload {
            operations: RawCbor::from(cbor::cbor_encode(&vec![lock_cancel(
                lock_id.clone(),
                Some(memo.clone()),
            )])),
        },
    };
    let summary = block_state
        .execute_transaction(&mut context, transaction_context, account_index_1, payload)
        .unwrap();
    assert_matches!(summary.outcome, TransactionOutcome::Rejected(TransactionRejectReason::NonExistentLockId(rejected_lock_id)) => {
        assert_eq!(rejected_lock_id, lock_id);
    });
}

/// Test that cancelling a lock is not blocked by token pause or deny-list restrictions.
#[test]
fn test_cancel_ignores_token_pause_and_deny_list() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let owner = context.external.create_account();
    let canceller = context.external.create_account();

    let token_id: TokenId = "pltX".parse().unwrap();
    let (gov_account, _token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default()
            .mintable()
            .burnable()
            .deny_list(),
        2,
        Some(RawTokenAmount::from(10000)),
    );

    let owner_addr = context
        .external
        .account_canonical_address(owner.account_index());
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        owner.account_index(),
        &token_id,
        RawTokenAmount::from(500),
    );

    let lock_id = LockId {
        account_index: owner.account_index().into(),
        sequence_number: 2,
        creation_order: 0,
    };
    let lock_config = utils::CreateLockSimpleConfig {
        recipients: vec![owner.account_index()],
        grants: vec![
            LockControllerSimpleV0Grant {
                account: owner.account_index(),
                roles: vec![LockControllerSimpleV0Capability::Fund],
            },
            LockControllerSimpleV0Grant {
                account: canceller.account_index(),
                roles: vec![LockControllerSimpleV0Capability::Cancel],
            },
        ],
        tokens: vec![token_id.clone()],
        expiry: 1000,
        keep_alive: false,
    };
    utils::create_lock(&mut context, &mut block_state, &lock_id, lock_config);

    let fund_events = utils::execute_meta_operations(
        &mut context,
        &mut block_state,
        owner.account_index(),
        vec![lock_fund(
            token_id.clone(),
            lock_id.clone(),
            concordium_base::protocol_level_tokens::TokenAmount::from_raw(500, 2),
            None,
        )],
    );
    assert_eq!(fund_events.len(), 1);

    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
        vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
            target: CborHolderAccount::from(owner_addr),
        })],
    );
    utils::pause_token(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
    );

    let events = utils::execute_meta_operations(
        &mut context,
        &mut block_state,
        canceller.account_index(),
        vec![lock_cancel(lock_id.clone(), None)],
    );
    assert_eq!(events.len(), 2);
    assert_matches!(&events[1], BlockItemEvent::LockDestroyed(LockDestroyEvent{lock_id: event_lock_id}) => {
        assert_eq!(event_lock_id, &lock_id);
    });
}
