//! Tests for cancelling a PLT lock.

use crate::utils::entity_traits::scheduler::SchedulerOperations;
use crate::utils::{BlockStateLatest, TokenInitTestParams};
use assert_matches::assert_matches;
use concordium_base::protocol_level_tokens::CborMemo;
use concordium_base::{
    base::Energy,
    common::cbor,
    protocol_level_locks::{LockControllerSimpleV0Capability, LockId},
    protocol_level_tokens::{
        RawCbor, TokenId,
        meta_operations::{MetaUpdatePayload, lock_cancel},
    },
    transactions::Payload,
};
use plt_block_state::{
    entity::entity_test_stub, persistent::protocol_level_locks::p11::LockControllerSimpleV0Grant,
};
use plt_block_state::entity::accounts::Accounts;
use plt_block_state::entity::block_state::LockNotFoundByIdError;
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
        Some(RawTokenAmount(10000)),
    );

    let lock_id = LockId {
        account_index: account_index_1.into(),
        sequence_number: 2,
        creation_order: 0,
    };
    utils::create_lock(
        &mut context,
        &mut block_state,
        &lock_id,
        vec![account_index_1],
        vec![LockControllerSimpleV0Grant {
            account: account_index_2,
            roles: vec![LockControllerSimpleV0Capability::Cancel],
        }],
        vec![plt_x.clone()],
        1000,
    );

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
        Some(RawTokenAmount(10000)),
    );

    let lock_id = LockId {
        account_index: account_index_1.into(),
        sequence_number: 2,
        creation_order: 0,
    };
    utils::create_lock(
        &mut context,
        &mut block_state,
        &lock_id,
        vec![account_index_1],
        vec![LockControllerSimpleV0Grant {
            account: account_index_1,
            roles: vec![LockControllerSimpleV0Capability::Cancel],
        }],
        vec![plt_x.clone()],
        1000,
    );

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
        Some(RawTokenAmount(10000)),
    );

    let lock_id = LockId {
        account_index: account_index_1.into(),
        sequence_number: 2,
        creation_order: 0,
    };
    utils::create_lock(
        &mut context,
        &mut block_state,
        &lock_id,
        vec![account_index_1],
        vec![LockControllerSimpleV0Grant {
            account: account_index_2,
            roles: vec![LockControllerSimpleV0Capability::Cancel],
        }],
        vec![plt_x.clone()],
        1000,
    );

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
        Some(RawTokenAmount(10000)),
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
        Some(RawTokenAmount(10000000)),
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
    utils::create_lock(
        &mut context,
        &mut block_state,
        &lock_id,
        vec![account_index_1],
        vec![
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
        vec![plt_x.clone()],
        1000,
    );
    utils::lock_balance(
        &mut context,
        &mut block_state,
        &lock_id,
        plt_x_gov_acct.account_index(),
        &plt_x.clone(),
        RawTokenAmount(500),
    );
    utils::lock_balance(
        &mut context,
        &mut block_state,
        &lock_id,
        plt_y_gov_acct.account_index(),
        &plt_y.clone(),
        RawTokenAmount(1000),
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
