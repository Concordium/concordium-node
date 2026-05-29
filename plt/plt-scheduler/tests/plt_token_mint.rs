//! Tests for token mint operations via the scheduler.

use crate::utils::TokenInitTestParams;
use crate::utils::entity_traits::scheduler::SchedulerOperations;
use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, DeserializationFailureRejectReason, MintWouldOverflowRejectReason,
    OperationNotPermittedRejectReason, RawCbor, TokenAdminRole, TokenAmount, TokenId,
    TokenModuleRejectReason, TokenOperation, TokenOperationsPayload, TokenSupplyUpdateDetails,
    TokenUpdateAdminRolesDetails, UnsupportedOperationRejectReason,
};
use concordium_base::transactions::Payload;
use plt_block_state::entity::block_state::p10::BlockStateP10;
use plt_block_state::entity::entity_test_stub;
use plt_scheduler_types::types::events::BlockItemEvent;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenHolder};

mod utils;

use crate::utils::BlockStateLatest;

/// Test successful mints on P10.
#[test]
fn test_mint_p10() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateP10::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p9(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );

    // First mint
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let gov_account_address = context
        .external
        .account_canonical_address(gov_account.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_account_address),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(1000)
    );

    // Second mint
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(4000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context_with_nonce(gov_account_address, 2),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(5000)
    );
}

/// Rejects mint operations from non-governance accounts on protocol version 10.
#[test]
fn test_unauthorized_mint_p10() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateP10::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p9(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let non_governance_account = context.external.create_account();

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let non_governance_account_addr = context
        .external
        .account_canonical_address(non_governance_account.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(non_governance_account_addr),
            non_governance_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
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
                Some(CborHolderAccount::from(non_governance_account_addr))
            );
        }
    );

    // Assert balances remain unchanged.
    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(0)
    );
    assert_eq!(
        non_governance_account.account_token_balance(&context, token_index),
        RawTokenAmount(0)
    );
}

/// Test successful mints.
#[test]
fn test_mint() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );

    // First mint
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_account_addr),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(1000)
    );

    // Second mint
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(4000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context_with_nonce(gov_account_addr, 2),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(5000)
    );
}

/// Rejects mint operations from non-governance accounts.
#[test]
fn test_unauthorized_mint() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let non_governance_account = context.external.create_account();

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let non_governance_account_addr = context
        .external
        .account_canonical_address(non_governance_account.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(non_governance_account_addr),
            non_governance_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
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
                Some(CborHolderAccount::from(non_governance_account_addr))
            );
        }
    );

    // Assert balances remain unchanged.
    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(0)
    );
    assert_eq!(
        non_governance_account.account_token_balance(&context, token_index),
        RawTokenAmount(0)
    );
}

/// Rejects mint operations from non-governance accounts using alias address.
/// Check that address in reject reason is the alias and not the canonical address.
#[test]
fn test_unauthorized_mint_using_alias() {
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
    let non_gov_account = context.external.create_account();
    let non_gov_account_address_alias = context
        .external
        .account_canonical_address(non_gov_account.account_index())
        .get_alias(5)
        .unwrap();

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(non_gov_account_address_alias),
            non_gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            address,
            ..
        }) => {
            // Assert the address alias is used in the reject reason.
            assert_eq!(
                address,
                Some(CborHolderAccount::from(non_gov_account_address_alias))
            );
        }
    );
}

/// Test mint that would overflow circulating supply.
#[test]
fn test_mint_overflow() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        Some(RawTokenAmount(1000)),
    );

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(RawTokenAmount::MAX.0 - 500, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_account_addr),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(reject_reason, TokenModuleRejectReason::MintWouldOverflow(
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

    // Supply unchanged
    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .unwrap();
    assert_eq!(
        token.token_base.token_circulating_supply(),
        RawTokenAmount(1000)
    );
}

/// Test mint with initial supply specified with wrong number of decimals.
#[test]
fn test_mint_decimals_mismatch() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        2,
        None,
    );

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_account_addr),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(reject_reason, TokenModuleRejectReason::DeserializationFailure(
        DeserializationFailureRejectReason {
            cause: Some(cause)
        }) => {
            assert!(cause.contains("decimals mismatch"), "cause: {}", cause);
    });
}

/// Reject "mint" operations while token is paused.
#[test]
fn test_mint_paused() {
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

    // Pause the token first
    utils::pause_token(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
    );

    // Now attempt to mint while paused
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_account_addr),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: 0,
            address: None,
            reason: Some(reason),
        }) if reason == "token operation mint is paused"
    );
}

/// Reject "mint" operation if the feature is not enabled.
#[test]
fn test_not_mintable() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        2,
        None,
    );

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(RawTokenAmount::MAX.0 - 500, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_account_addr),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::UnsupportedOperation(UnsupportedOperationRejectReason {
            index: 0,
            operation_type,
            reason: Some(reason)
        }) if reason == "feature not enabled" && operation_type == "mint"
    );
}

/// Test that mint events contain expected data.
#[test]
fn test_mint_event() {
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

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_account_addr),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenMint(mint) => {
        assert_eq!(mint.token_id, token_id);
        assert_eq!(mint.amount.amount, RawTokenAmount(1000));
        assert_eq!(mint.amount.decimals, 2);
        assert_eq!(mint.target, TokenHolder::Account(gov_account_addr));
    });
}

/// Rejects mint when governance account does not hold the mint role.
#[test]
fn test_reject_without_role() {
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

    // Revoke mint role from governance account.
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint],
                account: CborHolderAccount::from(gov_account_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_account_addr),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Attempting to mint as governance account (no longer has mint role).
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Mint(
            TokenSupplyUpdateDetails {
                amount: TokenAmount::from_raw(200, 2),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context_with_nonce(gov_account_addr, 2),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: 0,
            address: Some(address),
            reason: Some(reason),
        }) => {
            assert_eq!(reason, "sender is not authorized to perform the operation for this token");
            assert_eq!(address, CborHolderAccount::from(gov_account_addr));
        }
    );
}

/// Succeeds when another account holds the mint role.
#[test]
fn test_new_account_with_role_succeeds_mint() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let account2 = context.external.create_account();

    // Assign mint role to account2.
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AssignAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint],
                account: CborHolderAccount::from(
                    context
                        .external
                        .account_canonical_address(account2.account_index()),
                ),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_account_addr),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Mint as account2.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Mint(
            TokenSupplyUpdateDetails {
                amount: TokenAmount::from_raw(200, 2),
            },
        )])),
    };
    let account2_addr = context
        .external
        .account_canonical_address(account2.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(account2_addr),
            account2.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(0)
    );
    assert_eq!(
        account2.account_token_balance(&context, token_index),
        RawTokenAmount(200)
    );
}
