use crate::kernel_stub::{TokenInitTestParams, TransactionExecutionTestImpl};
use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, DeserializationFailureRejectReason, OperationNotPermittedRejectReason,
    RawCbor, TokenAmount, TokenBalanceInsufficientRejectReason, TokenModuleEventType,
    TokenModuleRejectReason, TokenOperation, TokenPauseDetails, TokenSupplyUpdateDetails,
};
use kernel_stub::KernelStub;
use plt_scheduler_interface::token_kernel_interface::TokenKernelQueries;
use plt_token_module::token_module::{self};
use plt_types::types::tokens::RawTokenAmount;

mod kernel_stub;
mod utils;

/// Test successful burns.
#[test]
fn test_burn() {
    let mut stub = KernelStub::with_decimals(2);
    let gov_account = stub.init_token(TokenInitTestParams::default());
    stub.set_account_balance(gov_account, RawTokenAmount(5000));

    // First burn
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    assert_eq!(
        stub.account_token_balance(&gov_account),
        RawTokenAmount(4000)
    );

    // Second burn
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(2000, 2),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    assert_eq!(
        stub.account_token_balance(&gov_account),
        RawTokenAmount(2000)
    );
}

/// Rejects burn operations from non-governance accounts.
#[test]
fn test_unauthorized_burn() {
    // Arrange a token and an unauthorized sender.
    let mut stub = KernelStub::with_decimals(2);
    stub.init_token(TokenInitTestParams::default());
    let non_governance_account = stub.create_account();
    stub.set_account_balance(non_governance_account, RawTokenAmount(5000));

    // Attempt to burn as a non-governance account.
    let mut execution = TransactionExecutionTestImpl::with_sender(non_governance_account);
    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    // Assert the operation is rejected with the unauthorized sender details.
    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index,
            address,
            ..
        }) => {
            assert_eq!(index, 0);
            assert_eq!(
                address,
                Some(CborHolderAccount::from(
                    stub.account_canonical_address(&non_governance_account)
                ))
            );
        }
    );

    // Assert the balance remains unchanged.
    assert_eq!(
        stub.account_token_balance(&non_governance_account),
        RawTokenAmount(5000)
    );

    // and that no events have been logged
    assert_eq!(stub.events.len(), 0);
}

/// Test burn amount that is not available on account.
#[test]
fn test_burn_insufficient_balance() {
    let mut stub = KernelStub::with_decimals(2);
    let gov_account = stub.init_token(TokenInitTestParams::default());
    stub.set_account_balance(gov_account, RawTokenAmount(1000));

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(2000, 2),
    })];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(reject_reason, TokenModuleRejectReason::TokenBalanceInsufficient(
        TokenBalanceInsufficientRejectReason {
            available_balance,
            required_balance,
            ..
        }) => {
            assert_eq!(required_balance, TokenAmount::from_raw(2000, 2));
            assert_eq!(available_balance, TokenAmount::from_raw(1000, 2));
    });
}

/// Test burn with amount specified with wrong number of decimals
#[test]
fn test_burn_decimals_mismatch() {
    let mut stub = KernelStub::with_decimals(2);
    let gov_account = stub.init_token(TokenInitTestParams::default());

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(reject_reason, TokenModuleRejectReason::DeserializationFailure(
        DeserializationFailureRejectReason {
            cause: Some(cause)
        }) => {
            assert!(cause.contains("decimals mismatch"), "cause: {}", cause);
    });
}

/// Reject "burn" operations while token is paused
#[test]
fn test_burn_paused() {
    let mut stub = KernelStub::with_decimals(2);
    let gov_account = stub.init_token(TokenInitTestParams::default());
    stub.set_account_balance(gov_account, RawTokenAmount(5000));

    // We set the token to be paused, and verify that the otherwise valid "burn" operation
    // is rejected in the subsequent transaction.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Pause(TokenPauseDetails {})];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("Executed successfully");

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: 0,
            address: None,
            reason: Some(reason),
        }) if reason == "token operation burn is paused"
    );

    assert_eq!(stub.events.len(), 1);
    assert_eq!(
        stub.events[0].0,
        TokenModuleEventType::Pause.to_type_discriminator()
    );
    assert!(stub.events[0].1.as_ref().is_empty());
}
