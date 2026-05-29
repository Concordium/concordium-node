//! Tests for the new `query_lock_list` and `query_lock_info` scheduler query functions.

use crate::utils::TokenInitTestParams;
use crate::utils::entity_traits::scheduler::SchedulerOperations;
use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::common::types::TransactionTime;
use concordium_base::protocol_level_locks::LockInfo;
use concordium_base::protocol_level_locks::{
    LockController, LockControllerSimpleV0Capability, LockId,
};
use concordium_base::protocol_level_tokens::{CborHolderAccount, TokenId};
use plt_block_state::entity::entity_test_stub;
use plt_block_state::persistent::protocol_level_locks::p11::LockControllerSimpleV0Grant;
use plt_scheduler::queries::QueryLockError;
use plt_scheduler_types::types::tokens::RawTokenAmount;

use crate::utils::BlockStateLatest;

mod utils;

/// `query_lock_info` produces the canonical CBOR encoding of the assembled
/// `LockInfo` value, and the round-trip via `cbor_decode` recovers an equal value.
#[test]
fn test_query_lock_info_cbor_round_trip_with_funded_balances() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let recipient = context.external.create_account();
    let funding_account = context.external.create_account();
    let recipient_addr = context
        .external
        .account_canonical_address(recipient.account_index());
    let funding_addr = context
        .external
        .account_canonical_address(funding_account.account_index());

    let token_id: TokenId = "TokenLockA".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        Some(RawTokenAmount(0)),
    );
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        funding_account.account_index(),
        &token_id,
        RawTokenAmount(123_400),
    );

    let lock_id = LockId {
        account_index: funding_account.account_index().into(),
        sequence_number: 1,
        creation_order: 0,
    };
    utils::create_lock(
        &mut context,
        &mut block_state,
        &lock_id,
        vec![recipient.account_index()],
        vec![LockControllerSimpleV0Grant {
            account: funding_account.account_index(),
            roles: vec![LockControllerSimpleV0Capability::Fund],
        }],
        vec![token_id.clone()],
        1_804_806_000,
    );
    utils::lock_balance(
        &mut context,
        &mut block_state,
        &lock_id,
        funding_account.account_index(),
        &token_id,
        RawTokenAmount(100),
    );

    let bytes = block_state
        .query_lock_info(&context, &lock_id)
        .expect("query_lock_info must succeed for an existing lock");
    let decoded: LockInfo =
        cbor::cbor_decode(&bytes).expect("CBOR encoding produced by query_lock_info must decode");

    assert_eq!(decoded.lock, lock_id);
    assert_eq!(
        decoded.recipients,
        vec![CborHolderAccount::from(recipient_addr)]
    );
    assert_eq!(decoded.expiry, TransactionTime::from(1_804_806_000));
    assert_matches!(decoded.controller, LockController::SimpleV0(simple) => {
        assert_eq!(simple.grants.len(), 1);
        assert_eq!(simple.grants[0].account, CborHolderAccount::from(funding_addr));
        assert_eq!(simple.grants[0].roles, vec![LockControllerSimpleV0Capability::Fund]);
        assert_eq!(simple.tokens, vec![token_id.clone()]);
        assert!(!simple.keep_alive);
        assert!(simple.memo.is_none());
    });
    assert_eq!(decoded.funds.len(), 1);
    assert_eq!(
        decoded.funds[0].account,
        CborHolderAccount::from(funding_addr)
    );
    assert_eq!(decoded.funds[0].amounts.len(), 1);
    assert_eq!(decoded.funds[0].amounts[0].token, token_id);
    assert_eq!(decoded.funds[0].amounts[0].amount.value(), 100);
    assert_eq!(decoded.funds[0].amounts[0].amount.decimals(), 2);
}

/// `query_lock_info` returns [`QueryLockError::LockDoesNotExist`] when the
/// lock id is not present in the block state.
#[test]
fn test_query_lock_info_unknown_lock() {
    let context = entity_test_stub::new_stubbed_context();
    let block_state = BlockStateLatest::default();
    let unknown = LockId {
        account_index: 999,
        sequence_number: 999,
        creation_order: 0,
    };
    let result = block_state.query_lock_info(&context, &unknown);
    assert_matches!(result, Err(QueryLockError::LockDoesNotExist));
}
