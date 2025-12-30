use crate::kernel_stub::TokenInitTestParams;
use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    AddressNotFoundRejectReason, CborHolderAccount, DeserializationFailureRejectReason, RawCbor,
    TokenAmount, TokenBalanceInsufficientRejectReason, TokenModuleRejectReasonType, TokenOperation,
    TokenTransfer,
};
use kernel_stub::KernelStub;
use plt_token_module::token_kernel_interface::{RawTokenAmount, TokenKernelQueries};
use plt_token_module::token_module::{self, TokenUpdateError, TransactionContext};

mod kernel_stub;

const NON_EXISTING_ACCOUNT: AccountAddress = AccountAddress([2u8; 32]);

/// Test successful transfer.
#[test]
fn test_transfer() {
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
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
        memo: None,
    })];
    token_module::execute_token_update_transaction(
        &mut stub,
        context,
        RawCbor::from(cbor::cbor_encode(&operations).unwrap()),
    )
    .expect("execute");

    assert_eq!(stub.account_balance(&sender), RawTokenAmount(4000));
    assert_eq!(stub.account_balance(&receiver), RawTokenAmount(3000));
}

/// Test transfer to sending account
#[test]
fn test_transfer_self() {
    let mut stub = KernelStub::new(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));

    let context = TransactionContext {
        sender,
        sender_address: stub.account_canonical_address(&sender),
    };
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&sender)),
        memo: None,
    })];
    token_module::execute_token_update_transaction(
        &mut stub,
        context,
        RawCbor::from(cbor::cbor_encode(&operations).unwrap()),
    )
    .expect("execute");

    assert_eq!(stub.account_balance(&sender), RawTokenAmount(5000));
}

/// Test transfer with unsufficient funds
#[test]
fn test_transfer_insufficient_balance() {
    let mut stub = KernelStub::new(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));

    let context = TransactionContext {
        sender,
        sender_address: stub.account_canonical_address(&sender),
    };
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(10000, 2),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
        memo: None,
    })];
    let res = token_module::execute_token_update_transaction(
        &mut stub,
        context,
        RawCbor::from(cbor::cbor_encode(&operations).unwrap()),
    );

    assert_matches!(
        &res,
        Err(TokenUpdateError::TokenModuleReject(reject_reason)) => {
            let reject_reason = reject_reason.decode_reject_reason().unwrap();

            assert_matches!(reject_reason, TokenModuleRejectReasonType::TokenBalanceInsufficient(
                TokenBalanceInsufficientRejectReason {
                    available_balance,
                    required_balance,
                    ..

                }) => {
                assert_eq!(available_balance, TokenAmount::from_raw(5000, 2));
                assert_eq!(required_balance, TokenAmount::from_raw(10000, 2));
            });

        }
    );
}

/// Test transfer with amount specified with wrong number of decimals
#[test]
fn test_transfer_decimals_mismatch() {
    let mut stub = KernelStub::new(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));

    let context = TransactionContext {
        sender,
        sender_address: stub.account_canonical_address(&sender),
    };
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 4),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
        memo: None,
    })];
    let res = token_module::execute_token_update_transaction(
        &mut stub,
        context,
        RawCbor::from(cbor::cbor_encode(&operations).unwrap()),
    );

    assert_matches!(
        &res,
        Err(TokenUpdateError::TokenModuleReject(reject_reason)) => {
            let reject_reason = reject_reason.decode_reject_reason().unwrap();

            assert_matches!(reject_reason, TokenModuleRejectReasonType::DeserializationFailure(
                DeserializationFailureRejectReason {
                    cause: Some(cause)
                }) => {
                assert!(cause.contains("decimals mismatch"), "cause: {}", cause);
            });

        }
    );
}

/// Test transfer where receiving account does not exist
#[test]
fn test_transfer_to_non_existing_receiver() {
    let mut stub = KernelStub::new(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));

    let context = TransactionContext {
        sender,
        sender_address: stub.account_canonical_address(&sender),
    };
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(NON_EXISTING_ACCOUNT),
        memo: None,
    })];
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
                    address,
                    ..
                }) => {
                assert_eq!(address.address, NON_EXISTING_ACCOUNT);
            });

        }
    );
}
