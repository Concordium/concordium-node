use crate::kernel_stub::{TokenInitTestParams, TransactionExecutionTestImpl};
use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    AddressNotFoundRejectReason, CborHolderAccount, CborMemo, DeserializationFailureRejectReason,
    OperationNotPermittedRejectReason, RawCbor, TokenAmount, TokenBalanceInsufficientRejectReason,
    TokenModuleEventType, TokenModuleRejectReason, TokenOperation, TokenPauseDetails,
    TokenTransfer,
};
use concordium_base::transactions::Memo;
use kernel_stub::KernelStub;
use plt_scheduler_interface::token_kernel_interface::TokenKernelQueries;
use plt_token_module::token_module::{self};
use plt_types::types::tokens::RawTokenAmount;

mod kernel_stub;
mod utils;

const NON_EXISTING_ACCOUNT: AccountAddress = AccountAddress([2u8; 32]);

/// Test successful transfer.
#[test]
fn test_transfer() {
    let mut stub = KernelStub::with_decimals(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));
    stub.set_account_balance(receiver, RawTokenAmount(2000));

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
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

    assert_eq!(stub.account_token_balance(&sender), RawTokenAmount(4000));
    assert_eq!(stub.account_token_balance(&receiver), RawTokenAmount(3000));
    let transfer = stub.pop_transfer().expect("transfer");
    assert_eq!(transfer.3, None);
}

/// Test successful transfer with memo.
#[test]
fn test_transfer_with_memo() {
    let mut stub = KernelStub::with_decimals(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let memo = Memo::try_from(cbor::cbor_encode("testvalue")).unwrap();
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(stub.account_address(&receiver)),
        memo: Some(CborMemo::Cbor(memo.clone())),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    assert_eq!(stub.account_token_balance(&sender), RawTokenAmount(4000));
    assert_eq!(stub.account_token_balance(&receiver), RawTokenAmount(1000));
    let transfer = stub.pop_transfer().expect("transfer");
    assert_eq!(transfer.3, Some(memo));
}

/// Test transfer to sending account
#[test]
fn test_transfer_self() {
    let mut stub = KernelStub::with_decimals(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(stub.account_address(&sender)),
        memo: None,
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    assert_eq!(stub.account_token_balance(&sender), RawTokenAmount(5000));
}

/// Test transfer with unsufficient funds
#[test]
fn test_transfer_insufficient_balance() {
    let mut stub = KernelStub::with_decimals(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(10000, 2),
        recipient: CborHolderAccount::from(stub.account_address(&receiver)),
        memo: None,
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
        assert_eq!(available_balance, TokenAmount::from_raw(5000, 2));
        assert_eq!(required_balance, TokenAmount::from_raw(10000, 2));
    });
}

/// Test transfer with amount specified with wrong number of decimals
#[test]
fn test_transfer_decimals_mismatch() {
    let mut stub = KernelStub::with_decimals(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 4),
        recipient: CborHolderAccount::from(stub.account_address(&receiver)),
        memo: None,
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

/// Test transfer where receiving account does not exist
#[test]
fn test_transfer_to_non_existing_receiver() {
    let mut stub = KernelStub::with_decimals(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(NON_EXISTING_ACCOUNT),
        memo: None,
    })];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);
    assert_matches!(reject_reason, TokenModuleRejectReason::AddressNotFound(
        AddressNotFoundRejectReason {
            address,
            ..
        }) => {
        assert_eq!(address.address, NON_EXISTING_ACCOUNT);
    });
}

/// Test transfer succeeds when both accounts are on allow list.
#[test]
fn test_transfer_allow_list_success() {
    let mut stub = KernelStub::with_decimals(2);
    let _gov_account = stub.init_token(TokenInitTestParams::default().allow_list());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));
    stub.set_account_balance(receiver, RawTokenAmount(2000));

    // Set up both accounts on the allow list.
    stub.set_allow_list(sender, true);
    stub.set_allow_list(receiver, true);

    // Transfer succeeds when both accounts are allow-listed.
    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
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
    .expect("Executed successfully");

    // Verify balances reflect the successful transfer.
    assert_eq!(stub.account_token_balance(&sender), RawTokenAmount(4000));
    assert_eq!(stub.account_token_balance(&receiver), RawTokenAmount(3000));
}

/// Test transfer succeeds when accounts are not on deny list.
#[test]
fn test_transfer_deny_list_success() {
    let mut stub = KernelStub::with_decimals(2);
    let _gov_account = stub.init_token(TokenInitTestParams::default().deny_list());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    let denied = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));
    stub.set_account_balance(receiver, RawTokenAmount(2000));

    // Put another account on the deny list to prove non-denied transfers work.
    stub.set_deny_list(denied, true);

    // Transfer succeeds when neither sender nor recipient is denied.
    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
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
    .expect("Executed successfully");

    // Verify balances reflect the successful transfer.
    assert_eq!(stub.account_token_balance(&sender), RawTokenAmount(4000));
    assert_eq!(stub.account_token_balance(&receiver), RawTokenAmount(3000));
}

/// Reject "transfer" operations when sender is not in allow list.
#[test]
fn test_transfer_sender_not_in_allow_list() {
    let mut stub = KernelStub::with_decimals(2);
    let _gov_account = stub.init_token(TokenInitTestParams::default().allow_list());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));
    stub.set_account_balance(receiver, RawTokenAmount(2000));

    stub.set_allow_list(receiver, true);

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(stub.account_address(&receiver)),
        memo: None,
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
            address: Some(address),
            reason: Some(reason),
        }) => {
            assert_eq!(address, CborHolderAccount::from(stub.account_address(&sender)));
            assert_eq!(reason, "sender not in allow list");
        }
    );

    assert_eq!(stub.account_token_balance(&sender), RawTokenAmount(5000));
    assert_eq!(stub.account_token_balance(&receiver), RawTokenAmount(2000));
}

/// Reject "transfer" operations when recipient is not in allow list.
#[test]
fn test_transfer_recipient_not_in_allow_list() {
    let mut stub = KernelStub::with_decimals(2);
    let _gov_account = stub.init_token(TokenInitTestParams::default().allow_list());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));
    stub.set_account_balance(receiver, RawTokenAmount(2000));

    stub.set_allow_list(sender, true);

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(stub.account_address(&receiver)),
        memo: None,
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
            address: Some(address),
            reason: Some(reason),
        }) => {
            assert_eq!(address, CborHolderAccount::from(stub.account_address(&receiver)));
            assert_eq!(reason, "recipient not in allow list");
        }
    );

    assert_eq!(stub.account_token_balance(&sender), RawTokenAmount(5000));
    assert_eq!(stub.account_token_balance(&receiver), RawTokenAmount(2000));
}

/// Reject "transfer" operations when sender is in deny list.
#[test]
fn test_transfer_sender_in_deny_list() {
    let mut stub = KernelStub::with_decimals(2);
    let _gov_account = stub.init_token(TokenInitTestParams::default().deny_list());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));
    stub.set_account_balance(receiver, RawTokenAmount(2000));

    stub.set_deny_list(sender, true);

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(stub.account_address(&receiver)),
        memo: None,
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
            address: Some(address),
            reason: Some(reason),
        }) => {
            assert_eq!(address, CborHolderAccount::from(stub.account_address(&sender)));
            assert_eq!(reason, "sender in deny list");
        }
    );

    assert_eq!(stub.account_token_balance(&sender), RawTokenAmount(5000));
    assert_eq!(stub.account_token_balance(&receiver), RawTokenAmount(2000));
}

/// Reject "transfer" operations when recipient is in deny list.
#[test]
fn test_transfer_recipient_in_deny_list() {
    let mut stub = KernelStub::with_decimals(2);
    let _gov_account = stub.init_token(TokenInitTestParams::default().deny_list());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));
    stub.set_account_balance(receiver, RawTokenAmount(2000));

    stub.set_deny_list(receiver, true);

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(stub.account_address(&receiver)),
        memo: None,
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
            address: Some(address),
            reason: Some(reason),
        }) => {
            assert_eq!(address, CborHolderAccount::from(stub.account_address(&receiver)));
            assert_eq!(reason, "recipient in deny list");
        }
    );

    assert_eq!(stub.account_token_balance(&sender), RawTokenAmount(5000));
    assert_eq!(stub.account_token_balance(&receiver), RawTokenAmount(2000));
}

/// Reject "transfer" operations while token is paused
#[test]
fn test_transfer_paused() {
    let mut stub = KernelStub::with_decimals(2);
    let gov_account = stub.init_token(TokenInitTestParams::default());
    let receiver = stub.create_account();
    stub.set_account_balance(gov_account, RawTokenAmount(5000));
    stub.set_account_balance(receiver, RawTokenAmount(2000));

    // We set the token to be paused, and verify that the otherwise valid "transfer" operation
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
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(stub.account_address(&receiver)),
        memo: None,
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
        }) if reason == "token operation transfer is paused"
    );

    assert_eq!(stub.events.len(), 1);
    assert_eq!(
        stub.events[0].0,
        TokenModuleEventType::Pause.to_type_discriminator()
    );
    assert!(stub.events[0].1.as_ref().is_empty());
}
