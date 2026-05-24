//! Tests for token burn operations via the scheduler.

use crate::utils::SchedulerOperations;
use crate::utils::TokenInitTestParams;
use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, DeserializationFailureRejectReason, OperationNotPermittedRejectReason,
    RawCbor, TokenAdminRole, TokenAmount, TokenId, TokenModuleRejectReason, TokenOperation,
    TokenOperationsPayload, TokenSupplyUpdateDetails, TokenUpdateAdminRolesDetails,
    UnsupportedOperationRejectReason,
};
use concordium_base::transactions::Payload;
use plt_block_state::entity::entity_test_stub;
use plt_scheduler_types::types::events::BlockItemEvent;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenHolder};

use crate::utils::BlockStateLatest;

mod utils;

/// Test successful burns.
#[test]
fn test_burn() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().burnable(),
        2,
        Some(RawTokenAmount(5000)),
    );

    // First burn
    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
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
            &gov_account,
            gov_account_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(4000)
    );

    // Second burn
    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(2000, 2),
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
            &gov_account,
            gov_account_addr,
            2.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(2000)
    );
}

/// Rejects burn operations from non-governance accounts.
/// The governance check is performed before the burnable feature check.
#[test]
fn test_unauthorized_burn() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().burnable(),
        2,
        None,
    );
    let non_governance_account = context.external.create_account();

    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let non_gov_addr = context
        .external
        .account_canonical_address(non_governance_account.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &non_governance_account,
            non_gov_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
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
                Some(CborHolderAccount::from(
                    context.external.account_canonical_address(non_governance_account.account_index())
                ))
            );
        }
    );
}

/// Test burn amount that exceeds account balance.
#[test]
fn test_burn_insufficient_balance() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().burnable(),
        2,
        Some(RawTokenAmount(1000)),
    );

    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(2000, 2),
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
            &gov_account,
            gov_account_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(reject_reason, TokenModuleRejectReason::TokenBalanceInsufficient(
        concordium_base::protocol_level_tokens::TokenBalanceInsufficientRejectReason {
            available_balance,
            required_balance,
            ..
        }) => {
            assert_eq!(required_balance, TokenAmount::from_raw(2000, 2));
            assert_eq!(available_balance, TokenAmount::from_raw(1000, 2));
    });
}

/// Test burn with amount specified with wrong number of decimals.
#[test]
fn test_burn_decimals_mismatch() {
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

    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
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
            &gov_account,
            gov_account_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
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

/// Reject "burn" operations while token is paused.
#[test]
fn test_burn_paused() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().burnable().mintable(),
        2,
        Some(RawTokenAmount(5000)),
    );

    utils::pause_token(&mut context, &mut block_state, &token_id, &gov_account);

    // Now attempt to burn while paused
    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
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
            &gov_account,
            gov_account_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
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
        }) if reason == "token operation burn is paused"
    );
}

/// Reject "burn" operation if the feature is not enabled.
#[test]
fn test_not_burnable() {
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

    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
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
            &gov_account,
            gov_account_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::UnsupportedOperation(UnsupportedOperationRejectReason {
            index: 0,
            operation_type,
            reason: Some(reason),
        }) if reason == "feature not enabled" && operation_type == "burn"
    );
}

/// Test that burn events contain expected data.
#[test]
fn test_burn_event() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().burnable(),
        2,
        Some(RawTokenAmount(5000)),
    );

    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
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
            &gov_account,
            gov_account_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenBurn(burn) => {
        assert_eq!(burn.token_id, token_id);
        assert_eq!(burn.amount.amount, RawTokenAmount(1000));
        assert_eq!(burn.amount.decimals, 2);
        assert_eq!(burn.target, TokenHolder::Account(context.external.account_canonical_address(gov_account.account_index())));
    });
}

/// Rejects burn when governance account does not hold the burn role.
#[test]
fn test_role_authorization_burn() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().burnable(),
        2,
        None,
    );

    // Revoke burn role from governance account.
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Burn],
                account: CborHolderAccount::from(gov_account_addr),
            },
        )])),
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
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Attempting to burn as governance account (no longer has burn role).
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Burn(
            TokenSupplyUpdateDetails {
                amount: TokenAmount::from_raw(200, 2),
            },
        )])),
    };
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &gov_account,
            gov_account_addr,
            2.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
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
            assert_eq!(address, CborHolderAccount::from(context.external.account_canonical_address(gov_account.account_index())));
        }
    );
}

/// Succeeds for another account holding the burn role.
#[test]
fn test_new_account_with_role_succeeds_burn() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().burnable().mintable(),
        2,
        None,
    );
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        gov_account.account_index(),
        &token_id,
        RawTokenAmount(10000),
    );
    let account2 = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        account2.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );

    // Assign burn role to account2.
    let account2_addr = context
        .external
        .account_canonical_address(account2.account_index());
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AssignAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Burn],
                account: CborHolderAccount::from(account2_addr),
            },
        )])),
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
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Burn as account2.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Burn(
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
            &account2,
            account2_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(10000)
    );
    assert_eq!(
        account2.account_token_balance(&context, token_index),
        RawTokenAmount(4800)
    );
}
