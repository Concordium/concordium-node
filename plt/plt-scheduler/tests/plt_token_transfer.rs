//! Tests for token transfer operations via the scheduler.

use crate::utils::SchedulerOperations;
use crate::utils::{BlockStateLatest, TokenInitTestParams};
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
use plt_block_state::entity::entity_test_stub;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::tokens::RawTokenAmount;

mod utils;

const NON_EXISTING_ACCOUNT: AccountAddress = AccountAddress([2u8; 32]);

#[test]
fn test_transfer() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = context.external.create_account();
    let receiver = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        sender.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        receiver.account_index(),
        &token_id,
        RawTokenAmount(2000),
    );

    let receiver_addr = context
        .external
        .account_canonical_address(receiver.account_index());
    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &sender,
            sender_addr,
            1.into(),
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
        sender.account_token_balance(&context, token_index),
        RawTokenAmount(4000)
    );
    assert_eq!(
        receiver.account_token_balance(&context, token_index),
        RawTokenAmount(3000)
    );
}

#[test]
fn test_transfer_with_memo() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = context.external.create_account();
    let receiver = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        sender.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );

    let memo = CborMemo::Cbor(Memo::try_from(cbor::cbor_encode("testvalue")).unwrap());
    let receiver_addr = context
        .external
        .account_canonical_address(receiver.account_index());
    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &sender,
            sender_addr,
            1.into(),
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
        sender.account_token_balance(&context, token_index),
        RawTokenAmount(4000)
    );
    assert_eq!(
        receiver.account_token_balance(&context, token_index),
        RawTokenAmount(1000)
    );
}

#[test]
fn test_transfer_self() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        sender.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );

    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &sender,
            sender_addr,
            1.into(),
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
        sender.account_token_balance(&context, token_index),
        RawTokenAmount(5000)
    );
}

#[test]
fn test_transfer_insufficient_balance() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = context.external.create_account();
    let receiver = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        sender.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );

    let receiver_addr = context
        .external
        .account_canonical_address(receiver.account_index());
    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &sender,
            sender_addr,
            1.into(),
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
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = context.external.create_account();
    let receiver = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        sender.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );

    let receiver_addr = context
        .external
        .account_canonical_address(receiver.account_index());
    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &sender,
            sender_addr,
            1.into(),
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
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        sender.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );

    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &sender,
            sender_addr,
            1.into(),
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
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        2,
        Some(RawTokenAmount(5000)),
    );
    let receiver = context.external.create_account();

    // Gov is NOT auto-added to allow list — must add explicitly.
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        &gov_account,
        vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
            target: CborHolderAccount::from(gov_account_addr),
        })],
    );
    let receiver_addr = context
        .external
        .account_canonical_address(receiver.account_index());
    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        &gov_account,
        vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
            target: CborHolderAccount::from(receiver_addr),
        })],
    );

    let result = block_state
        .execute_transaction(
            &mut context,
            &gov_account,
            gov_account_addr,
            1.into(),
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
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(4000)
    );
    assert_eq!(
        receiver.account_token_balance(&context, token_index),
        RawTokenAmount(1000)
    );
}

#[test]
fn test_transfer_deny_list_success() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().deny_list().mintable(),
        2,
        None,
    );
    let sender = context.external.create_account();
    let receiver = context.external.create_account();
    let denied = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        sender.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        receiver.account_index(),
        &token_id,
        RawTokenAmount(2000),
    );

    let denied_addr = context
        .external
        .account_canonical_address(denied.account_index());
    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        &gov_account,
        vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
            target: CborHolderAccount::from(denied_addr),
        })],
    );

    let receiver_addr = context
        .external
        .account_canonical_address(receiver.account_index());
    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &sender,
            sender_addr,
            1.into(),
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
        sender.account_token_balance(&context, token_index),
        RawTokenAmount(4000)
    );
    assert_eq!(
        receiver.account_token_balance(&context, token_index),
        RawTokenAmount(3000)
    );
}

#[test]
fn test_transfer_sender_not_in_allow_list() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        2,
        Some(RawTokenAmount(5000)),
    );
    let sender = context.external.create_account();
    let receiver = context.external.create_account();

    let receiver_addr = context
        .external
        .account_canonical_address(receiver.account_index());
    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        &gov_account,
        vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
            target: CborHolderAccount::from(receiver_addr),
        })],
    );

    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &sender,
            sender_addr,
            1.into(),
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
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        2,
        Some(RawTokenAmount(5000)),
    );
    let receiver = context.external.create_account();

    // Gov is NOT auto-added to allow list — add gov so it can transfer,
    // but do NOT add receiver — transfer should fail with "recipient not in allow list".
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        &gov_account,
        vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
            target: CborHolderAccount::from(gov_account_addr),
        })],
    );

    let receiver_addr = context
        .external
        .account_canonical_address(receiver.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &gov_account,
            gov_account_addr,
            1.into(),
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
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(5000)
    );
    assert_eq!(
        receiver.account_token_balance(&context, token_index),
        RawTokenAmount(0)
    );
}

#[test]
fn test_transfer_sender_in_deny_list() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().deny_list().mintable(),
        2,
        None,
    );
    let sender = context.external.create_account();
    let receiver = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        sender.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        receiver.account_index(),
        &token_id,
        RawTokenAmount(2000),
    );

    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        &gov_account,
        vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
            target: CborHolderAccount::from(sender_addr),
        })],
    );

    let receiver_addr = context
        .external
        .account_canonical_address(receiver.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &sender,
            sender_addr,
            1.into(),
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
        sender.account_token_balance(&context, token_index),
        RawTokenAmount(5000)
    );
    assert_eq!(
        receiver.account_token_balance(&context, token_index),
        RawTokenAmount(2000)
    );
}

#[test]
fn test_transfer_recipient_in_deny_list() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().deny_list().mintable(),
        2,
        None,
    );
    let sender = context.external.create_account();
    let receiver = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        sender.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        receiver.account_index(),
        &token_id,
        RawTokenAmount(2000),
    );

    let receiver_addr = context
        .external
        .account_canonical_address(receiver.account_index());
    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        &gov_account,
        vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
            target: CborHolderAccount::from(receiver_addr),
        })],
    );

    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &sender,
            sender_addr,
            1.into(),
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
        sender.account_token_balance(&context, token_index),
        RawTokenAmount(5000)
    );
    assert_eq!(
        receiver.account_token_balance(&context, token_index),
        RawTokenAmount(2000)
    );
}

#[test]
fn test_transfer_paused() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let receiver = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        gov_account.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );

    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let pause_ops = vec![TokenOperation::Pause(TokenPauseDetails {})];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&pause_ops)),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            &gov_account,
            gov_account_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("pause");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let receiver_addr = context
        .external
        .account_canonical_address(receiver.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &gov_account,
            gov_account_addr,
            2.into(),
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
