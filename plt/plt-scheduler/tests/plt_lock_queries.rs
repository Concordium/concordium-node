//! Tests for the new `query_lock_list` and `query_lock_info` scheduler query functions.

use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::common::types::TransactionTime;
use concordium_base::protocol_level_locks::LockInfo;
use concordium_base::protocol_level_locks::{
    LockController, LockControllerSimpleV0Capability, LockId,
};
use concordium_base::protocol_level_tokens::{CborHolderAccount, TokenId};
use plt_scheduler::queries::{self, QueryLockError};
use plt_scheduler_types::types::locks::LockControllerSimpleV0Grant;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use utils::block_state_external_stubbed::{
    BlockStateWithExternalStateStubbed, TokenInitTestParams,
};

mod utils;

/// `query_lock_info` produces the canonical CBOR encoding of the assembled
/// `LockInfo` value, and the round-trip via `cbor_decode` recovers an equal value.
#[test]
fn test_query_lock_info_cbor_round_trip_with_funded_balances() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);

    let recipient = stub.create_account();
    let funding_account = stub.create_account();
    let recipient_addr = stub.account_canonical_address(&recipient);
    let funding_addr = stub.account_canonical_address(&funding_account);

    let token_id: TokenId = "TokenLockA".parse().unwrap();
    let (token, _gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        Some(RawTokenAmount(0)),
    );
    stub.increment_account_balance(funding_account, token, RawTokenAmount(123_400));

    let lock_id = LockId {
        account_index: recipient.index,
        sequence_number: 2,
        creation_order: 0,
    };
    stub.create_lock(
        &lock_id,
        vec![recipient],
        vec![LockControllerSimpleV0Grant {
            account: funding_account,
            roles: vec![LockControllerSimpleV0Capability::Fund],
        }],
        vec![token_id.clone()],
        1_804_806_000,
    );
    // Track the (account, token) pair under the lock and record the locked amount in
    // the token-module key-value state so it shows up in `lock_balances` / `funds`.
    stub.lock_balance(&lock_id, &funding_account, &token, RawTokenAmount(100));

    let bytes = queries::query_lock_info(stub.state(), &lock_id)
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
    let stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let unknown = LockId {
        account_index: 999,
        sequence_number: 999,
        creation_order: 0,
    };
    let result = queries::query_lock_info(stub.state(), &unknown);
    assert_matches!(result, Err(QueryLockError::LockDoesNotExist));
}
