use crate::kernel_stub::TokenInitTestParams;
use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    AddressNotFoundRejectReason, CborHolderAccount, DeserializationFailureRejectReason, RawCbor,
    TokenAmount, TokenModuleRejectReasonType, TokenOperation, TokenTransfer,
};
use kernel_stub::KernelStub;
use plt_token_module::token_kernel_interface::{RawTokenAmount, TokenKernelQueries};
use plt_token_module::token_module::{self, TokenUpdateError, TransactionContext};

mod kernel_stub;

const NON_EXISTING_ACCOUNT: AccountAddress = AccountAddress([2u8; 32]);

/// Test failure to decode token operations
#[test]
fn test_update_token_decode_failure() {
    let mut stub = KernelStub::new(0);
    let sender = stub.create_account();
    let context = TransactionContext {
        sender,
        sender_address: stub.account_canonical_address(&sender),
    };
    let res =
        token_module::execute_token_update_transaction(&mut stub, context, RawCbor::from(vec![]));
    assert_matches!(
        &res,
        Err(TokenUpdateError::TokenModuleReject(reject_reason)) => {
            let reject_reason = reject_reason.decode_reject_reason().unwrap();

            assert_matches!(reject_reason, TokenModuleRejectReasonType::DeserializationFailure(
                DeserializationFailureRejectReason {
                    cause: Some(cause)
                }) => {
                assert!(cause.contains("IO error"), "cause: {}", cause);
            });

        }
    );
}

/// Test transaction with multiple operations
#[test]
fn test_multiple_operations() {
    let mut stub = KernelStub::new(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));
    stub.set_account_balance(receiver, RawTokenAmount(2000));

    let context = TransactionContext {
        sender,
        sender_address: stub.account_canonical_address(&sender),
    };
    let operations = vec![
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(1000, 2),
            recipient: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
            memo: None,
        }),
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(2000, 2),
            recipient: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
            memo: None,
        }),
    ];
    token_module::execute_token_update_transaction(
        &mut stub,
        context,
        RawCbor::from(cbor::cbor_encode(&operations).unwrap()),
    )
    .expect("execute");

    assert_eq!(stub.account_balance(&sender), RawTokenAmount(2000));
    assert_eq!(stub.account_balance(&receiver), RawTokenAmount(5000));
}

/// Test transaction with multiple operations where one of them fail.
#[test]
fn test_single_failing_operation() {
    let mut stub = KernelStub::new(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));

    let context = TransactionContext {
        sender,
        sender_address: stub.account_canonical_address(&sender),
    };
    let operations = vec![
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(1000, 2),
            recipient: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
            memo: None,
        }),
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(2000, 2),
            recipient: CborHolderAccount::from(NON_EXISTING_ACCOUNT),
            memo: None,
        }),
    ];
    let res = token_module::execute_token_update_transaction(
        &mut stub,
        context,
        RawCbor::from(cbor::cbor_encode(&operations).unwrap()),
    );

    assert_matches!(
        &res,
        Err(TokenUpdateError::TokenModuleReject(reject_reason)) => {
            let reject_reason = reject_reason.decode_reject_reason().unwrap();

            assert_matches!(reject_reason, TokenModuleRejectReasonType::AddressNotFound(
                AddressNotFoundRejectReason {
                    index,
                    address,
                }) => {
                assert_eq!(address.address, NON_EXISTING_ACCOUNT);
                assert_eq!(index, 1);
            });

        }
    );
}
