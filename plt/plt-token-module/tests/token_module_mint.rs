use crate::kernel_stub::{TokenInitTestParams, TransactionExecutionTestImpl};
use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    DeserializationFailureRejectReason, MintWouldOverflowRejectReason,
    OperationNotPermittedRejectReason, RawCbor, TokenAmount, TokenModuleEventType,
    TokenModuleRejectReasonEnum, TokenOperation, TokenPauseDetails, TokenSupplyUpdateDetails,
};
use kernel_stub::KernelStub;
use plt_scheduler_interface::token_kernel_interface::TokenKernelQueries;
use plt_token_module::token_module::{self};
use plt_types::types::primitives::RawTokenAmount;

mod kernel_stub;
mod utils;

/// Test successful mints.
#[test]
fn test_mint() {
    let mut stub = KernelStub::with_decimals(2);
    let gov_account = stub.init_token(TokenInitTestParams::default());

    // First mint
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
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
        RawTokenAmount(1000)
    );

    // Second mint
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(4000, 2),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    assert_eq!(
        stub.account_token_balance(&gov_account),
        RawTokenAmount(5000)
    );
}

/// Test mint that would overflow circulating supply
#[test]
fn test_mint_overflow() {
    let mut stub = KernelStub::with_decimals(2);
    let gov_account = stub.init_token(TokenInitTestParams::default());
    stub.set_account_balance(gov_account, RawTokenAmount(1000));

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(RawTokenAmount::MAX.0 - 500, 2),
    })];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(reject_reason, TokenModuleRejectReasonEnum::MintWouldOverflow(
        MintWouldOverflowRejectReason {
            requested_amount,
            current_supply,
            max_representable_amount,
            ..
        }) => {
            assert_eq!(requested_amount, TokenAmount::from_raw(RawTokenAmount::MAX.0 - 500, 2));
            assert_eq!(current_supply, TokenAmount::from_raw(1000, 2));
            assert_eq!(max_representable_amount, TokenAmount::from_raw(RawTokenAmount::MAX.0, 2));
    });
}

/// Test mint with initial supply specified with wrong number of decimals
#[test]
fn test_mint_decimals_mismatch() {
    let mut stub = KernelStub::with_decimals(2);
    let gov_account = stub.init_token(TokenInitTestParams::default());

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(reject_reason, TokenModuleRejectReasonEnum::DeserializationFailure(
        DeserializationFailureRejectReason {
            cause: Some(cause)
        }) => {
            assert!(cause.contains("decimals mismatch"), "cause: {}", cause);
    });
}

/// Reject "mint" operations while token is paused
#[test]
fn test_mint_paused() {
    let mut stub = KernelStub::with_decimals(2);
    let gov_account = stub.init_token(TokenInitTestParams::default());

    // We set the token to be paused, and verify that the otherwise valid "mint" operation
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
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
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
        TokenModuleRejectReasonEnum::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: 0,
            address: None,
            reason: Some(reason),
        }) if reason == "token operation mint is paused"
    );

    assert_eq!(stub.events.len(), 1);
    assert_eq!(
        stub.events[0].0,
        TokenModuleEventType::Pause.to_type_discriminator()
    );
    assert!(stub.events[0].1.as_ref().is_empty());
}
