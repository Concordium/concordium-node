//! Tests for token transfer operations via the scheduler.

use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    AddressNotFoundRejectReason, CborHolderAccount, CborMemo, DeserializationFailureRejectReason,
    OperationNotPermittedRejectReason, RawCbor, TokenAmount, TokenBalanceInsufficientRejectReason,
    TokenId, TokenListUpdateDetails, TokenModuleRejectReason, TokenOperation,
    TokenOperationsPayload, TokenPauseDetails, TokenTransfer,
};
use concordium_base::transactions::{Memo, Payload};
use plt_block_state::block_state_interface::BlockStateQuery;
use plt_scheduler::scheduler;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use utils::block_state_external_stubbed::{
    BlockStateWithExternalStateStubbed, TokenInitTestParams,
};

mod utils;

const NON_EXISTING_ACCOUNT: AccountAddress = AccountAddress([2u8; 32]);

#[test]
fn test_transfer() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, _gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.increment_account_balance(sender, token, RawTokenAmount(5000));
    stub.increment_account_balance(receiver, token, RawTokenAmount(2000));

    let receiver_addr = stub.account_canonical_address(&receiver);
    let sender_addr = stub.account_canonical_address(&sender);
    let result = scheduler::execute_transaction(
        sender,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate {
            payload: TokenOperationsPayload {
                token_id: token_id.clone(),
                operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Transfer(
                    TokenTransfer {
                        amount: TokenAmount::from_raw(1000, 2),
                        recipient: CborHolderAccount::from(receiver_addr),
                        memo: None,
                    },
                )])),
            },
        },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));
    assert_eq!(
        stub.state().account_token_balance(&sender, &token),
        RawTokenAmount(4000)
    );
    assert_eq!(
        stub.state().account_token_balance(&receiver, &token),
        RawTokenAmount(3000)
    );
}

#[test]
fn test_transfer_with_memo() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, _gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.increment_account_balance(sender, token, RawTokenAmount(5000));

    let memo = CborMemo::Cbor(Memo::try_from(cbor::cbor_encode("testvalue")).unwrap());
    let receiver_addr = stub.account_canonical_address(&receiver);
    let sender_addr = stub.account_canonical_address(&sender);
    let result = scheduler::execute_transaction(
        sender,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate {
            payload: TokenOperationsPayload {
                token_id: token_id.clone(),
                operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Transfer(
                    TokenTransfer {
                        amount: TokenAmount::from_raw(1000, 2),
                        recipient: CborHolderAccount::from(receiver_addr),
                        memo: Some(memo),
                    },
                )])),
            },
        },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));
    assert_eq!(
        stub.state().account_token_balance(&sender, &token),
        RawTokenAmount(4000)
    );
    assert_eq!(
        stub.state().account_token_balance(&receiver, &token),
        RawTokenAmount(1000)
    );
}

#[test]
fn test_transfer_self() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, _gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = stub.create_account();
    stub.increment_account_balance(sender, token, RawTokenAmount(5000));

    let sender_addr = stub.account_canonical_address(&sender);
    let result = scheduler::execute_transaction(
        sender,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate {
            payload: TokenOperationsPayload {
                token_id: token_id.clone(),
                operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Transfer(
                    TokenTransfer {
                        amount: TokenAmount::from_raw(1000, 2),
                        recipient: CborHolderAccount::from(sender_addr),
                        memo: None,
                    },
                )])),
            },
        },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));
    assert_eq!(
        stub.state().account_token_balance(&sender, &token),
        RawTokenAmount(5000)
    );
}

#[test]
fn test_transfer_insufficient_balance() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, _gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.increment_account_balance(sender, token, RawTokenAmount(5000));

    let receiver_addr = stub.account_canonical_address(&receiver);
    let sender_addr = stub.account_canonical_address(&sender);
    let result = scheduler::execute_transaction(
        sender,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate {
            payload: TokenOperationsPayload {
                token_id: token_id.clone(),
                operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Transfer(
                    TokenTransfer {
                        amount: TokenAmount::from_raw(10000, 2),
                        recipient: CborHolderAccount::from(receiver_addr),
                        memo: None,
                    },
                )])),
            },
        },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(reject_reason, TokenModuleRejectReason::TokenBalanceInsufficient(
        TokenBalanceInsufficientRejectReason { available_balance, required_balance, .. }) => {
        assert_eq!(available_balance, TokenAmount::from_raw(5000, 2));
        assert_eq!(required_balance, TokenAmount::from_raw(10000, 2));
    });
}

#[test]
fn test_transfer_decimals_mismatch() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, _gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.increment_account_balance(sender, token, RawTokenAmount(5000));

    let receiver_addr = stub.account_canonical_address(&receiver);
    let sender_addr = stub.account_canonical_address(&sender);
    let result = scheduler::execute_transaction(
        sender,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate {
            payload: TokenOperationsPayload {
                token_id: token_id.clone(),
                operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Transfer(
                    TokenTransfer {
                        amount: TokenAmount::from_raw(1000, 4),
                        recipient: CborHolderAccount::from(receiver_addr),
                        memo: None,
                    },
                )])),
            },
        },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(reject_reason, TokenModuleRejectReason::DeserializationFailure(
        DeserializationFailureRejectReason { cause: Some(cause) }) => {
        assert!(cause.contains("decimals mismatch"), "cause: {}", cause);
    });
}

#[test]
fn test_transfer_to_non_existing_receiver() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, _gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = stub.create_account();
    stub.increment_account_balance(sender, token, RawTokenAmount(5000));

    let sender_addr = stub.account_canonical_address(&sender);
    let result = scheduler::execute_transaction(
        sender,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate {
            payload: TokenOperationsPayload {
                token_id: token_id.clone(),
                operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Transfer(
                    TokenTransfer {
                        amount: TokenAmount::from_raw(1000, 2),
                        recipient: CborHolderAccount::from(NON_EXISTING_ACCOUNT),
                        memo: None,
                    },
                )])),
            },
        },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(reject_reason, TokenModuleRejectReason::AddressNotFound(
        AddressNotFoundRejectReason { address, .. }) => {
        assert_eq!(address.address, NON_EXISTING_ACCOUNT);
    });
}

#[test]
fn test_transfer_allow_list_success() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        2,
        Some(RawTokenAmount(5000)),
    );
    let receiver = stub.create_account();

    // Gov is NOT auto-added to allow list — must add explicitly.
    let gov_addr = stub.account_canonical_address(&gov_account);
    stub.execute_token_operations(
        &token_id,
        gov_account,
        vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
            target: CborHolderAccount::from(gov_addr),
        })],
    );
    let receiver_addr = stub.account_canonical_address(&receiver);
    stub.execute_token_operations(
        &token_id,
        gov_account,
        vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
            target: CborHolderAccount::from(receiver_addr),
        })],
    );

    let result = scheduler::execute_transaction(
        gov_account,
        gov_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate {
            payload: TokenOperationsPayload {
                token_id: token_id.clone(),
                operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Transfer(
                    TokenTransfer {
                        amount: TokenAmount::from_raw(1000, 2),
                        recipient: CborHolderAccount::from(receiver_addr),
                        memo: None,
                    },
                )])),
            },
        },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));
    assert_eq!(
        stub.state().account_token_balance(&gov_account, &token),
        RawTokenAmount(4000)
    );
    assert_eq!(
        stub.state().account_token_balance(&receiver, &token),
        RawTokenAmount(1000)
    );
}

#[test]
fn test_transfer_deny_list_success() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().deny_list().mintable(),
        2,
        None,
    );
    let sender = stub.create_account();
    let receiver = stub.create_account();
    let denied = stub.create_account();
    stub.increment_account_balance(sender, token, RawTokenAmount(5000));
    stub.increment_account_balance(receiver, token, RawTokenAmount(2000));

    let denied_addr = stub.account_canonical_address(&denied);
    stub.execute_token_operations(
        &token_id,
        gov_account,
        vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
            target: CborHolderAccount::from(denied_addr),
        })],
    );

    let sender_addr = stub.account_canonical_address(&sender);
    let receiver_addr = stub.account_canonical_address(&receiver);
    let result = scheduler::execute_transaction(
        sender,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate {
            payload: TokenOperationsPayload {
                token_id: token_id.clone(),
                operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Transfer(
                    TokenTransfer {
                        amount: TokenAmount::from_raw(1000, 2),
                        recipient: CborHolderAccount::from(receiver_addr),
                        memo: None,
                    },
                )])),
            },
        },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));
    assert_eq!(
        stub.state().account_token_balance(&sender, &token),
        RawTokenAmount(4000)
    );
    assert_eq!(
        stub.state().account_token_balance(&receiver, &token),
        RawTokenAmount(3000)
    );
}

#[test]
fn test_transfer_sender_not_in_allow_list() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        2,
        Some(RawTokenAmount(5000)),
    );
    let sender = stub.create_account();
    let receiver = stub.create_account();

    let receiver_addr = stub.account_canonical_address(&receiver);
    stub.execute_token_operations(
        &token_id,
        gov_account,
        vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
            target: CborHolderAccount::from(receiver_addr),
        })],
    );

    let sender_addr = stub.account_canonical_address(&sender);
    let result = scheduler::execute_transaction(
        sender,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate {
            payload: TokenOperationsPayload {
                token_id: token_id.clone(),
                operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Transfer(
                    TokenTransfer {
                        amount: TokenAmount::from_raw(1000, 2),
                        recipient: CborHolderAccount::from(receiver_addr),
                        memo: None,
                    },
                )])),
            },
        },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: 0, address: Some(address), reason: Some(reason),
        }) => {
            assert_eq!(address, CborHolderAccount::from(sender_addr));
            assert_eq!(reason, "sender not in allow list");
        }
    );
}

#[test]
fn test_transfer_recipient_not_in_allow_list() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        2,
        Some(RawTokenAmount(5000)),
    );
    let receiver = stub.create_account();

    // Gov is NOT auto-added to allow list — add gov so it can transfer,
    // but do NOT add receiver — transfer should fail with "recipient not in allow list".
    let gov_addr = stub.account_canonical_address(&gov_account);
    stub.execute_token_operations(
        &token_id,
        gov_account,
        vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
            target: CborHolderAccount::from(gov_addr),
        })],
    );

    let receiver_addr = stub.account_canonical_address(&receiver);
    let result = scheduler::execute_transaction(
        gov_account,
        gov_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate {
            payload: TokenOperationsPayload {
                token_id: token_id.clone(),
                operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Transfer(
                    TokenTransfer {
                        amount: TokenAmount::from_raw(1000, 2),
                        recipient: CborHolderAccount::from(receiver_addr),
                        memo: None,
                    },
                )])),
            },
        },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: 0, address: Some(address), reason: Some(reason),
        }) => {
            assert_eq!(address, CborHolderAccount::from(receiver_addr));
            assert_eq!(reason, "recipient not in allow list");
        }
    );
    assert_eq!(
        stub.state().account_token_balance(&gov_account, &token),
        RawTokenAmount(5000)
    );
    assert_eq!(
        stub.state().account_token_balance(&receiver, &token),
        RawTokenAmount(0)
    );
}

#[test]
fn test_transfer_sender_in_deny_list() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().deny_list().mintable(),
        2,
        None,
    );
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.increment_account_balance(sender, token, RawTokenAmount(5000));
    stub.increment_account_balance(receiver, token, RawTokenAmount(2000));

    let sender_addr = stub.account_canonical_address(&sender);
    stub.execute_token_operations(
        &token_id,
        gov_account,
        vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
            target: CborHolderAccount::from(sender_addr),
        })],
    );

    let receiver_addr = stub.account_canonical_address(&receiver);
    let result = scheduler::execute_transaction(
        sender,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate {
            payload: TokenOperationsPayload {
                token_id: token_id.clone(),
                operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Transfer(
                    TokenTransfer {
                        amount: TokenAmount::from_raw(1000, 2),
                        recipient: CborHolderAccount::from(receiver_addr),
                        memo: None,
                    },
                )])),
            },
        },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: 0, address: Some(address), reason: Some(reason),
        }) => {
            assert_eq!(address, CborHolderAccount::from(sender_addr));
            assert_eq!(reason, "sender in deny list");
        }
    );
    assert_eq!(
        stub.state().account_token_balance(&sender, &token),
        RawTokenAmount(5000)
    );
    assert_eq!(
        stub.state().account_token_balance(&receiver, &token),
        RawTokenAmount(2000)
    );
}

#[test]
fn test_transfer_recipient_in_deny_list() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().deny_list().mintable(),
        2,
        None,
    );
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.increment_account_balance(sender, token, RawTokenAmount(5000));
    stub.increment_account_balance(receiver, token, RawTokenAmount(2000));

    let receiver_addr = stub.account_canonical_address(&receiver);
    stub.execute_token_operations(
        &token_id,
        gov_account,
        vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
            target: CborHolderAccount::from(receiver_addr),
        })],
    );

    let sender_addr = stub.account_canonical_address(&sender);
    let result = scheduler::execute_transaction(
        sender,
        sender_addr,
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate {
            payload: TokenOperationsPayload {
                token_id: token_id.clone(),
                operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Transfer(
                    TokenTransfer {
                        amount: TokenAmount::from_raw(1000, 2),
                        recipient: CborHolderAccount::from(receiver_addr),
                        memo: None,
                    },
                )])),
            },
        },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: 0, address: Some(address), reason: Some(reason),
        }) => {
            assert_eq!(address, CborHolderAccount::from(receiver_addr));
            assert_eq!(reason, "recipient in deny list");
        }
    );
    assert_eq!(
        stub.state().account_token_balance(&sender, &token),
        RawTokenAmount(5000)
    );
    assert_eq!(
        stub.state().account_token_balance(&receiver, &token),
        RawTokenAmount(2000)
    );
}

#[test]
fn test_transfer_paused() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let receiver = stub.create_account();
    stub.increment_account_balance(gov_account, token, RawTokenAmount(5000));

    let gov_addr = stub.account_canonical_address(&gov_account);
    let pause_ops = vec![TokenOperation::Pause(TokenPauseDetails {})];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&pause_ops)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        gov_addr,
        1.into(),

        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("pause");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let receiver_addr = stub.account_canonical_address(&receiver);
    let result = scheduler::execute_transaction(
        gov_account,
        gov_addr,
        2.into(),
        stub.state_mut(),
        Payload::TokenUpdate {
            payload: TokenOperationsPayload {
                token_id: token_id.clone(),
                operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Transfer(
                    TokenTransfer {
                        amount: TokenAmount::from_raw(1000, 2),
                        recipient: CborHolderAccount::from(receiver_addr),
                        memo: None,
                    },
                )])),
            },
        },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: 0, address: None, reason: Some(reason),
        }) if reason == "token operation transfer is paused"
    );
}
