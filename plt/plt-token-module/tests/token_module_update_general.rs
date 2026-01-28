use crate::kernel_stub::{TokenInitTestParams, TransactionExecutionTestImpl};
use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    AddressNotFoundRejectReason, CborHolderAccount, DeserializationFailureRejectReason, RawCbor,
    TokenAmount, TokenModuleRejectReason, TokenOperation, TokenTransfer,
};
use kernel_stub::KernelStub;
use plt_scheduler_interface::token_kernel_interface::TokenKernelQueries;
use plt_token_module::token_module::{self, TokenUpdateError};
use plt_types::types::tokens::RawTokenAmount;

mod kernel_stub;
mod utils;

const NON_EXISTING_ACCOUNT: AccountAddress = AccountAddress([2u8; 32]);

/// Test failure to decode token operations
#[test]
fn test_update_token_decode_failure() {
    let mut stub = KernelStub::with_decimals(0);
    let sender = stub.create_account();
    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(vec![]),
    );

    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(reject_reason, TokenModuleRejectReason::DeserializationFailure(
        DeserializationFailureRejectReason {
            cause: Some(cause)
        }) => {
        assert!(cause.contains("IO error"), "cause: {}", cause);
    });
}

/// Test transaction with multiple operations
#[test]
fn test_multiple_operations() {
    let mut stub = KernelStub::with_decimals(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));
    stub.set_account_balance(receiver, RawTokenAmount(2000));

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let operations = vec![
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(1000, 2),
            recipient: CborHolderAccount::from(stub.account_address(&receiver)),
            memo: None,
        }),
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(2000, 2),
            recipient: CborHolderAccount::from(stub.account_address(&receiver)),
            memo: None,
        }),
    ];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    assert_eq!(stub.account_token_balance(&sender), RawTokenAmount(2000));
    assert_eq!(stub.account_token_balance(&receiver), RawTokenAmount(5000));
}

/// Test transaction with multiple operations where one of them fail.
#[test]
fn test_single_failing_operation() {
    let mut stub = KernelStub::with_decimals(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let operations = vec![
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(1000, 2),
            recipient: CborHolderAccount::from(stub.account_address(&receiver)),
            memo: None,
        }),
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(2000, 2),
            recipient: CborHolderAccount::from(NON_EXISTING_ACCOUNT),
            memo: None,
        }),
    ];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(reject_reason, TokenModuleRejectReason::AddressNotFound(
        AddressNotFoundRejectReason {
            index,
            address,
        }) => {
        assert_eq!(address.address, NON_EXISTING_ACCOUNT);
        assert_eq!(index, 1);
    });
}

/// Test that energy is charged for execution of operations.
#[test]
fn test_energy_charge() {
    let mut stub = KernelStub::with_decimals(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));
    stub.set_account_balance(receiver, RawTokenAmount(2000));

    let mut execution =
        TransactionExecutionTestImpl::with_sender_and_energy(sender, Energy::from(1000));
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(stub.account_address(&receiver)),
        memo: None,
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    // Assert energy was charged
    assert_eq!(execution.remaining_energy().energy, 1000 - 100);
}

/// Test hitting out of energy error.
#[test]
fn test_out_of_energy_error() {
    let mut stub = KernelStub::with_decimals(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));
    stub.set_account_balance(receiver, RawTokenAmount(2000));

    let mut execution =
        TransactionExecutionTestImpl::with_sender_and_energy(sender, Energy::from(50));
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(stub.account_address(&receiver)),
        memo: None,
    })];
    let result = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    assert_matches!(result, Err(TokenUpdateError::OutOfEnergy(_)));
}
