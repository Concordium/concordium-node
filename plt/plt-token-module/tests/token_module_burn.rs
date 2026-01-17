use crate::kernel_stub::{TokenInitTestParams, TransactionExecutionTestImpl};
use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    DeserializationFailureRejectReason, RawCbor, TokenAmount, TokenBalanceInsufficientRejectReason,
    TokenModuleRejectReasonEnum, TokenOperation, TokenSupplyUpdateDetails,
};
use kernel_stub::KernelStub;
use plt_token_module::token_kernel_interface::{RawTokenAmount, TokenKernelQueries};
use plt_token_module::token_module::{self};

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
    assert_matches!(reject_reason, TokenModuleRejectReasonEnum::TokenBalanceInsufficient(
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
    assert_matches!(reject_reason, TokenModuleRejectReasonEnum::DeserializationFailure(
        DeserializationFailureRejectReason {
            cause: Some(cause)
        }) => {
            assert!(cause.contains("decimals mismatch"), "cause: {}", cause);
    });
}
