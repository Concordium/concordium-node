//! Tests for token pause/unpause operations via the scheduler.

use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, OperationNotPermittedRejectReason, RawCbor, TokenAdminRole, TokenAmount,
    TokenId, TokenModuleEventType, TokenModuleRejectReason, TokenModuleState, TokenOperation,
    TokenOperationsPayload, TokenPauseDetails, TokenPauseEventDetails, TokenSupplyUpdateDetails,
    TokenUpdateAdminRolesDetails,
};
use concordium_base::transactions::Payload;
use plt_block_state::block_state_interface::BlockStateQuery;
use plt_scheduler::{queries, scheduler};
use plt_scheduler_types::types::events::BlockItemEvent;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use utils::block_state_external_stubbed::{
    BlockStateWithExternalStateStubbed, TokenInitTestParams,
};

mod utils;

/// Test that pause/unpause operations modify the token module state as expected.
#[test]
fn test_token_pause_state() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 0, None);

    assert!(!{
        let info = queries::query_token_info(stub.state(), &token_id).unwrap();
        let state: TokenModuleState = cbor::cbor_decode(&info.state.module_state).unwrap();
        state.paused.unwrap_or(false)
    });

    // Pause the token
    let events = stub.execute_token_operations(
        &token_id,
        gov_account,
        vec![TokenOperation::Pause(TokenPauseDetails {})],
    );
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenModule(event) => {
        assert_eq!(event.event_type, TokenModuleEventType::Pause.to_type_discriminator());
        let _details: TokenPauseEventDetails = cbor::cbor_decode(&event.details).unwrap();
    });
    assert!({
        let info = queries::query_token_info(stub.state(), &token_id).unwrap();
        let state: TokenModuleState = cbor::cbor_decode(&info.state.module_state).unwrap();
        state.paused.unwrap_or(false)
    });

    // Unpause the token
    let unpause_ops = vec![TokenOperation::Unpause(TokenPauseDetails {})];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&unpause_ops)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenModule(event) => {
        assert_eq!(event.event_type, TokenModuleEventType::Unpause.to_type_discriminator());
        let _details: TokenPauseEventDetails = cbor::cbor_decode(&event.details).unwrap();
    });
    assert!(!{
        let info = queries::query_token_info(stub.state(), &token_id).unwrap();
        let state: TokenModuleState = cbor::cbor_decode(&info.state.module_state).unwrap();
        state.paused.unwrap_or(false)
    });
}

/// Performing a double pause within one transaction and then again in another is permitted.
#[test]
fn test_double_pause() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 0, None);

    // Double pause in one transaction
    let operations = vec![
        TokenOperation::Pause(TokenPauseDetails {}),
        TokenOperation::Pause(TokenPauseDetails {}),
    ];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);
    assert_eq!(events.len(), 2);
    for event in &events {
        assert_matches!(event, BlockItemEvent::TokenModule(e) => {
            assert_eq!(e.event_type, TokenModuleEventType::Pause.to_type_discriminator());
        });
    }

    // Pause again in a subsequent transaction
    let events = stub.execute_token_operations(
        &token_id,
        gov_account,
        vec![TokenOperation::Pause(TokenPauseDetails {})],
    );
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenModule(e) => {
        assert_eq!(e.event_type, TokenModuleEventType::Pause.to_type_discriminator());
    });

    assert!({
        let info = queries::query_token_info(stub.state(), &token_id).unwrap();
        let state: TokenModuleState = cbor::cbor_decode(&info.state.module_state).unwrap();
        state.paused.unwrap_or(false)
    });
}

/// Performing an unpause when the token is not paused is permitted.
#[test]
fn test_redundant_unpause() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 0, None);

    // Token is already unpaused; unpause again
    let unpause_ops = vec![TokenOperation::Unpause(TokenPauseDetails {})];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&unpause_ops)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenModule(event) => {
        assert_eq!(event.event_type, TokenModuleEventType::Unpause.to_type_discriminator());
    });
    assert!(!{
        let info = queries::query_token_info(stub.state(), &token_id).unwrap();
        let state: TokenModuleState = cbor::cbor_decode(&info.state.module_state).unwrap();
        state.paused.unwrap_or(false)
    });
}

/// Rejects pause operations from non-governance accounts.
#[test]
fn test_unauthorized_pause() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, _gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 0, None);
    let non_governance_account = stub.create_account();

    let operations = vec![TokenOperation::Pause(TokenPauseDetails {})];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        non_governance_account,
        stub.account_canonical_address(non_governance_account),
        stub.state_mut(),
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
                    stub.account_canonical_address(non_governance_account)
                ))
            );
        }
    );

    // Token must remain unpaused
    assert!(!{
        let info = queries::query_token_info(stub.state(), &token_id).unwrap();
        let state: TokenModuleState = cbor::cbor_decode(&info.state.module_state).unwrap();
        state.paused.unwrap_or(false)
    });
}

/// Rejects unpause operations from non-governance accounts.
#[test]
fn test_unauthorized_unpause() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 0, None);
    let non_governance_account = stub.create_account();

    // Gov pauses the token first
    stub.execute_token_operations(
        &token_id,
        gov_account,
        vec![TokenOperation::Pause(TokenPauseDetails {})],
    );
    assert!({
        let info = queries::query_token_info(stub.state(), &token_id).unwrap();
        let state: TokenModuleState = cbor::cbor_decode(&info.state.module_state).unwrap();
        state.paused.unwrap_or(false)
    });

    // Non-gov attempts to unpause
    let operations = vec![TokenOperation::Unpause(TokenPauseDetails {})];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        non_governance_account,
        stub.account_canonical_address(non_governance_account),
        stub.state_mut(),
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
                    stub.account_canonical_address(non_governance_account)
                ))
            );
        }
    );

    // Token must remain paused
    assert!({
        let info = queries::query_token_info(stub.state(), &token_id).unwrap();
        let state: TokenModuleState = cbor::cbor_decode(&info.state.module_state).unwrap();
        state.paused.unwrap_or(false)
    });
}

/// A transaction [Pause, Mint] is rejected because Mint is not permitted while paused.
///
/// Semantic note: In the scheduler, the token module key-value state is a local copy that is
/// only committed on transaction success. Therefore the Pause takes effect within the same
/// transaction's local state, but since the transaction ultimately fails (at Mint), the local
/// state is discarded. The token is NOT paused and NO events are emitted after this rejection.
#[test]
fn test_pause_multiple_ops() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );

    let operations = vec![
        TokenOperation::Pause(TokenPauseDetails {}),
        TokenOperation::Mint(TokenSupplyUpdateDetails {
            amount: TokenAmount::from_raw(1000, 2),
        }),
    ];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: 1,
            address: None,
            reason: Some(reason),
        }) if reason == "token operation mint is paused"
    );

    // No tokens minted
    assert_eq!(
        stub.state().token_circulating_supply(token),
        RawTokenAmount(0)
    );
    // Token is NOT paused (local state was discarded on rejection)
    assert!(!{
        let info = queries::query_token_info(stub.state(), &token_id).unwrap();
        let state: TokenModuleState = cbor::cbor_decode(&info.state.module_state).unwrap();
        state.paused.unwrap_or(false)
    });
}

/// A transaction [Unpause, Mint] succeeds: unpause takes effect first, then mint proceeds.
#[test]
fn test_unpause_multiple_ops() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );

    // Pause the token first
    stub.execute_token_operations(
        &token_id,
        gov_account,
        vec![TokenOperation::Pause(TokenPauseDetails {})],
    );
    assert!({
        let info = queries::query_token_info(stub.state(), &token_id).unwrap();
        let state: TokenModuleState = cbor::cbor_decode(&info.state.module_state).unwrap();
        state.paused.unwrap_or(false)
    });

    // [Unpause, Mint] in one transaction
    let operations = vec![
        TokenOperation::Unpause(TokenPauseDetails {}),
        TokenOperation::Mint(TokenSupplyUpdateDetails {
            amount: TokenAmount::from_raw(1000, 2),
        }),
    ];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // 2 events: Unpause + Mint
    assert_eq!(events.len(), 2);
    assert_matches!(&events[0], BlockItemEvent::TokenModule(e) => {
        assert_eq!(e.event_type, TokenModuleEventType::Unpause.to_type_discriminator());
    });
    assert_matches!(&events[1], BlockItemEvent::TokenMint(_));

    assert!(!{
        let info = queries::query_token_info(stub.state(), &token_id).unwrap();
        let state: TokenModuleState = cbor::cbor_decode(&info.state.module_state).unwrap();
        state.paused.unwrap_or(false)
    });
    assert_eq!(
        stub.state().token_circulating_supply(token),
        RawTokenAmount(1000)
    );
}

/// Rejects pause when governance account does not hold the pause role.
#[test]
fn test_role_authorization_pause() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 0, None);

    // Revoke pause role from governance account.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Pause],
                account: CborHolderAccount::from(stub.account_canonical_address(gov_account)),
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Attempting to pause as governance account (no longer has pause role).
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Pause(
            TokenPauseDetails {},
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(gov_account),
        stub.state_mut(),
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
            assert_eq!(address, CborHolderAccount::from(stub.account_canonical_address(gov_account)));
        }
    );
    // Token must remain unpaused.
    assert!(!{
        let info = queries::query_token_info(stub.state(), &token_id).unwrap();
        let state: TokenModuleState = cbor::cbor_decode(&info.state.module_state).unwrap();
        state.paused.unwrap_or(false)
    });
}

/// Succeeds for another account holding the pause role.
#[test]
fn test_new_account_with_role_succeeds_pause() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 0, None);
    let account2 = stub.create_account();

    // Assign pause role to account2.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AssignAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Pause],
                account: CborHolderAccount::from(stub.account_canonical_address(account2)),
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Pause as account2.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Pause(
            TokenPauseDetails {},
        )])),
    };
    let result = scheduler::execute_transaction(
        account2,
        stub.account_canonical_address(account2),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    assert!({
        let info = queries::query_token_info(stub.state(), &token_id).unwrap();
        let state: TokenModuleState = cbor::cbor_decode(&info.state.module_state).unwrap();
        state.paused.unwrap_or(false)
    });
}
