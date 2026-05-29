//! Tests for token allow/deny list update operations via the scheduler.

use crate::utils::TokenInitTestParams;
use crate::utils::entity_traits::scheduler::SchedulerOperations;
use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, OperationNotPermittedRejectReason, RawCbor, TokenAdminRole, TokenId,
    TokenListUpdateDetails, TokenListUpdateEventDetails, TokenModuleAccountState,
    TokenModuleEventType, TokenModuleRejectReason, TokenOperation, TokenOperationsPayload,
    TokenUpdateAdminRolesDetails, UnsupportedOperationRejectReason,
};
use concordium_base::transactions::Payload;
use plt_block_state::entity::entity_test_stub;
use plt_scheduler_types::types::events::BlockItemEvent;
use plt_scheduler_types::types::execution::TransactionOutcome;

use crate::utils::BlockStateLatest;

mod utils;

/// Test allow list add then remove.
#[test]
fn test_allow_list_updates() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        0,
        None,
    );
    let target_account = context.external.create_account();

    // Target account not yet touched
    assert!(
        block_state
            .query_token_account_infos(&context, target_account.account_index())
            .unwrap()
            .is_empty()
    );

    let target_addr = context
        .external
        .account_canonical_address(target_account.account_index());

    // Add to allow list
    let events = utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
        vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
            target: CborHolderAccount::from(target_addr),
        })],
    );
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenModule(event) => {
        assert_eq!(event.event_type, TokenModuleEventType::AddAllowList.to_type_discriminator());
        let add_event: TokenListUpdateEventDetails = cbor::cbor_decode(&event.details).unwrap();
        assert_eq!(add_event.target, CborHolderAccount::from(target_addr));
    });
    let infos = block_state
        .query_token_account_infos(&context, target_account.account_index())
        .unwrap();
    let module_state = infos[0].account_state.module_state.as_ref().unwrap();
    let state: TokenModuleAccountState = cbor::cbor_decode(module_state).unwrap();
    assert_eq!(state.allow_list, Some(true));
    assert_eq!(state.deny_list, None);

    // Remove from allow list
    let events = utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
        vec![TokenOperation::RemoveAllowList(TokenListUpdateDetails {
            target: CborHolderAccount::from(target_addr),
        })],
    );
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenModule(event) => {
        assert_eq!(event.event_type, TokenModuleEventType::RemoveAllowList.to_type_discriminator());
        let remove_event: TokenListUpdateEventDetails = cbor::cbor_decode(&event.details).unwrap();
        assert_eq!(remove_event.target, CborHolderAccount::from(target_addr));
    });
    let infos = block_state
        .query_token_account_infos(&context, target_account.account_index())
        .unwrap();
    let module_state = infos[0].account_state.module_state.as_ref().unwrap();
    let state: TokenModuleAccountState = cbor::cbor_decode(module_state).unwrap();
    assert_eq!(state.allow_list, Some(false));
    assert_eq!(state.deny_list, None);
}

/// Test deny list add then remove.
#[test]
fn test_deny_list_updates() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().deny_list(),
        0,
        None,
    );
    let target_account = context.external.create_account();

    // Target account not yet touched
    assert!(
        block_state
            .query_token_account_infos(&context, target_account.account_index())
            .unwrap()
            .is_empty()
    );

    let target_addr = context
        .external
        .account_canonical_address(target_account.account_index());

    // Add to deny list
    let events = utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
        vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
            target: CborHolderAccount::from(target_addr),
        })],
    );
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenModule(event) => {
        assert_eq!(event.event_type, TokenModuleEventType::AddDenyList.to_type_discriminator());
        let add_event: TokenListUpdateEventDetails = cbor::cbor_decode(&event.details).unwrap();
        assert_eq!(add_event.target, CborHolderAccount::from(target_addr));
    });
    let infos = block_state
        .query_token_account_infos(&context, target_account.account_index())
        .unwrap();
    let module_state = infos[0].account_state.module_state.as_ref().unwrap();
    let state: TokenModuleAccountState = cbor::cbor_decode(module_state).unwrap();
    assert_eq!(state.allow_list, None);
    assert_eq!(state.deny_list, Some(true));

    // Remove from deny list
    let events = utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
        vec![TokenOperation::RemoveDenyList(TokenListUpdateDetails {
            target: CborHolderAccount::from(target_addr),
        })],
    );
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenModule(event) => {
        assert_eq!(event.event_type, TokenModuleEventType::RemoveDenyList.to_type_discriminator());
        let remove_event: TokenListUpdateEventDetails = cbor::cbor_decode(&event.details).unwrap();
        assert_eq!(remove_event.target, CborHolderAccount::from(target_addr));
    });
    let infos = block_state
        .query_token_account_infos(&context, target_account.account_index())
        .unwrap();
    let module_state = infos[0].account_state.module_state.as_ref().unwrap();
    let state: TokenModuleAccountState = cbor::cbor_decode(module_state).unwrap();
    assert_eq!(state.allow_list, None);
    assert_eq!(state.deny_list, Some(false));
}

/// Non-governance account cannot add to allow list.
#[test]
fn test_add_allow_list_reject_non_governance() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        0,
        None,
    );
    let sender = context.external.create_account();
    let target_account = context.external.create_account();

    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let target_addr = context
        .external
        .account_canonical_address(target_account.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AddAllowList(
            TokenListUpdateDetails {
                target: CborHolderAccount::from(target_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(sender_addr),
            sender.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            address: Some(address),
            reason: Some(reason),
            ..
        }) => {
            assert_eq!(address, CborHolderAccount::from(sender_addr));
            assert_eq!(reason, "sender is not authorized to perform the operation for this token");
        }
    );

    // Target account must remain untouched
    assert!(
        block_state
            .query_token_account_infos(&context, target_account.account_index())
            .unwrap()
            .is_empty()
    );
}

/// Non-governance account cannot remove from allow list.
#[test]
fn test_remove_allow_list_reject_non_governance() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        0,
        None,
    );
    let sender = context.external.create_account();
    let target_account = context.external.create_account();

    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let target_addr = context
        .external
        .account_canonical_address(target_account.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RemoveAllowList(
            TokenListUpdateDetails {
                target: CborHolderAccount::from(target_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(sender_addr),
            sender.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            address: Some(address),
            reason: Some(reason),
            ..
        }) => {
            assert_eq!(address, CborHolderAccount::from(sender_addr));
            assert_eq!(reason, "sender is not authorized to perform the operation for this token");
        }
    );

    assert!(
        block_state
            .query_token_account_infos(&context, target_account.account_index())
            .unwrap()
            .is_empty()
    );
}

/// Non-governance account cannot add to deny list.
#[test]
fn test_add_deny_list_reject_non_governance() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().deny_list(),
        0,
        None,
    );
    let sender = context.external.create_account();
    let target_account = context.external.create_account();

    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let target_addr = context
        .external
        .account_canonical_address(target_account.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AddDenyList(
            TokenListUpdateDetails {
                target: CborHolderAccount::from(target_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(sender_addr),
            sender.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            address: Some(address),
            reason: Some(reason),
            ..
        }) => {
            assert_eq!(address, CborHolderAccount::from(sender_addr));
            assert_eq!(reason, "sender is not authorized to perform the operation for this token");
        }
    );

    assert!(
        block_state
            .query_token_account_infos(&context, target_account.account_index())
            .unwrap()
            .is_empty()
    );
}

/// Non-governance account cannot remove from deny list.
#[test]
fn test_remove_deny_list_reject_non_governance() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().deny_list(),
        0,
        None,
    );
    let sender = context.external.create_account();
    let target_account = context.external.create_account();

    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let target_addr = context
        .external
        .account_canonical_address(target_account.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RemoveDenyList(
            TokenListUpdateDetails {
                target: CborHolderAccount::from(target_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(sender_addr),
            sender.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            address: Some(address),
            reason: Some(reason),
            ..
        }) => {
            assert_eq!(address, CborHolderAccount::from(sender_addr));
            assert_eq!(reason, "sender is not authorized to perform the operation for this token");
        }
    );

    assert!(
        block_state
            .query_token_account_infos(&context, target_account.account_index())
            .unwrap()
            .is_empty()
    );
}

/// AddAllowList touches the target account.
#[test]
fn test_add_allow_list_touches_account() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        0,
        None,
    );
    let target_account = context.external.create_account();

    assert!(
        block_state
            .query_token_account_infos(&context, target_account.account_index())
            .unwrap()
            .is_empty()
    );

    let target_addr = context
        .external
        .account_canonical_address(target_account.account_index());
    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
        vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
            target: CborHolderAccount::from(target_addr),
        })],
    );

    assert!(
        !block_state
            .query_token_account_infos(&context, target_account.account_index())
            .unwrap()
            .is_empty()
    );
}

/// RemoveAllowList touches the target account.
#[test]
fn test_remove_allow_list_touches_account() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        0,
        None,
    );
    let target_account = context.external.create_account();

    assert!(
        block_state
            .query_token_account_infos(&context, target_account.account_index())
            .unwrap()
            .is_empty()
    );

    let target_addr = context
        .external
        .account_canonical_address(target_account.account_index());
    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
        vec![TokenOperation::RemoveAllowList(TokenListUpdateDetails {
            target: CborHolderAccount::from(target_addr),
        })],
    );

    assert!(
        !block_state
            .query_token_account_infos(&context, target_account.account_index())
            .unwrap()
            .is_empty()
    );
}

/// AddDenyList touches the target account.
#[test]
fn test_add_deny_list_touches_account() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().deny_list(),
        0,
        None,
    );
    let target_account = context.external.create_account();

    assert!(
        block_state
            .query_token_account_infos(&context, target_account.account_index())
            .unwrap()
            .is_empty()
    );

    let target_addr = context
        .external
        .account_canonical_address(target_account.account_index());
    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
        vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
            target: CborHolderAccount::from(target_addr),
        })],
    );

    assert!(
        !block_state
            .query_token_account_infos(&context, target_account.account_index())
            .unwrap()
            .is_empty()
    );
}

/// RemoveDenyList touches the target account.
#[test]
fn test_remove_deny_list_touches_account() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().deny_list(),
        0,
        None,
    );
    let target_account = context.external.create_account();

    assert!(
        block_state
            .query_token_account_infos(&context, target_account.account_index())
            .unwrap()
            .is_empty()
    );

    let target_addr = context
        .external
        .account_canonical_address(target_account.account_index());
    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
        vec![TokenOperation::RemoveDenyList(TokenListUpdateDetails {
            target: CborHolderAccount::from(target_addr),
        })],
    );

    assert!(
        !block_state
            .query_token_account_infos(&context, target_account.account_index())
            .unwrap()
            .is_empty()
    );
}

/// Adding to allow list fails when the allow list feature is not enabled.
#[test]
fn test_add_to_not_enabled_allow_list() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(), // allow_list not enabled
        0,
        None,
    );
    let allow_account = context.external.create_account();

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let allow_addr = context
        .external
        .account_canonical_address(allow_account.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AddAllowList(
            TokenListUpdateDetails {
                target: CborHolderAccount::from(allow_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_addr),
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
            reason: Some(reason),
        }) if reason == "feature not enabled" && operation_type == "addAllowList"
    );
}

/// Removing from allow list fails when the allow list feature is not enabled.
#[test]
fn test_remove_from_not_enabled_allow_list() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(), // allow_list not enabled
        0,
        None,
    );
    let allow_account = context.external.create_account();

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let allow_addr = context
        .external
        .account_canonical_address(allow_account.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RemoveAllowList(
            TokenListUpdateDetails {
                target: CborHolderAccount::from(allow_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_addr),
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
            reason: Some(reason),
        }) if reason == "feature not enabled" && operation_type == "removeAllowList"
    );
}

/// Adding to deny list fails when the deny list feature is not enabled.
#[test]
fn test_add_to_not_enabled_deny_list() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(), // deny_list not enabled
        0,
        None,
    );
    let deny_account = context.external.create_account();

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let deny_addr = context
        .external
        .account_canonical_address(deny_account.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AddDenyList(
            TokenListUpdateDetails {
                target: CborHolderAccount::from(deny_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_addr),
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
            reason: Some(reason),
        }) if reason == "feature not enabled" && operation_type == "addDenyList"
    );
}

/// Removing from deny list fails when the deny list feature is not enabled.
#[test]
fn test_remove_from_not_enabled_deny_list() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(), // deny_list not enabled
        0,
        None,
    );
    let deny_account = context.external.create_account();

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let deny_addr = context
        .external
        .account_canonical_address(deny_account.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RemoveDenyList(
            TokenListUpdateDetails {
                target: CborHolderAccount::from(deny_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_addr),
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
            reason: Some(reason),
        }) if reason == "feature not enabled" && operation_type == "removeDenyList"
    );
}

/// Rejects AddDenyList when governance account does not hold the updateDenyList role.
#[test]
fn test_reject_add_denylist_without_role() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().deny_list(),
        0,
        None,
    );

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    // Revoke updateDenyList role from governance account.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::UpdateDenyList],
                account: CborHolderAccount::from(gov_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_addr),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Attempting to add to deny list as governance account.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AddDenyList(
            TokenListUpdateDetails {
                target: CborHolderAccount::from(gov_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context_with_nonce(gov_addr, 2),
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
            assert_eq!(address, CborHolderAccount::from(gov_addr));
        }
    );
}

/// Rejects AddAllowList when governance account does not hold the updateAllowList role.
#[test]
fn test_reject_add_allowlist_without_role() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        0,
        None,
    );

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    // Revoke updateAllowList role.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::UpdateAllowList],
                account: CborHolderAccount::from(gov_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_addr),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Attempting to add to allow list.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AddAllowList(
            TokenListUpdateDetails {
                target: CborHolderAccount::from(gov_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context_with_nonce(gov_addr, 2),
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
            assert_eq!(address, CborHolderAccount::from(gov_addr));
        }
    );
}

/// Rejects RemoveDenyList when governance account does not hold the updateDenyList role.
#[test]
fn test_reject_remove_denylist_without_role() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().deny_list(),
        0,
        None,
    );

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    // First add gov to deny list, then revoke the role.
    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
        vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
            target: CborHolderAccount::from(gov_addr),
        })],
    );

    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::UpdateDenyList],
                account: CborHolderAccount::from(gov_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_addr),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Attempting to remove from deny list.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RemoveDenyList(
            TokenListUpdateDetails {
                target: CborHolderAccount::from(gov_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context_with_nonce(gov_addr, 2),
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
            assert_eq!(address, CborHolderAccount::from(gov_addr));
        }
    );
}

/// Rejects RemoveAllowList when governance account does not hold the updateAllowList role.
#[test]
fn test_reject_remove_allowlist_without_role() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        0,
        None,
    );

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    // First add gov to allow list, then revoke the role.
    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
        vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
            target: CborHolderAccount::from(gov_addr),
        })],
    );

    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::UpdateAllowList],
                account: CborHolderAccount::from(gov_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_addr),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Attempting to remove from allow list.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RemoveAllowList(
            TokenListUpdateDetails {
                target: CborHolderAccount::from(gov_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context_with_nonce(gov_addr, 2),
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
            assert_eq!(address, CborHolderAccount::from(gov_addr));
        }
    );
}

/// Succeeds for another account holding the updateDenyList role.
#[test]
fn test_succeeds_add_deny_list_new_account_with_role() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().deny_list(),
        0,
        None,
    );
    let account2 = context.external.create_account();

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let account2_addr = context
        .external
        .account_canonical_address(account2.account_index());

    // Assign updateDenyList role to account2.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AssignAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::UpdateDenyList],
                account: CborHolderAccount::from(account2_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_addr),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Add gov to deny list as account2.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AddDenyList(
            TokenListUpdateDetails {
                target: CborHolderAccount::from(gov_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(account2_addr),
            account2.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let infos = block_state
        .query_token_account_infos(&context, gov_account.account_index())
        .unwrap();
    let module_state = infos[0].account_state.module_state.as_ref().unwrap();
    let state: TokenModuleAccountState = cbor::cbor_decode(module_state).unwrap();
    assert_eq!(state.deny_list, Some(true));
}

/// Succeeds for another account holding the updateAllowList role.
#[test]
fn test_succeeds_add_allow_list_new_account_with_role() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        0,
        None,
    );
    let account2 = context.external.create_account();

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let account2_addr = context
        .external
        .account_canonical_address(account2.account_index());

    // Assign updateAllowList role to account2.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AssignAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::UpdateAllowList],
                account: CborHolderAccount::from(account2_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(gov_addr),
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Add gov to allow list as account2.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AddAllowList(
            TokenListUpdateDetails {
                target: CborHolderAccount::from(gov_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(account2_addr),
            account2.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let infos = block_state
        .query_token_account_infos(&context, gov_account.account_index())
        .unwrap();
    let module_state = infos[0].account_state.module_state.as_ref().unwrap();
    let state: TokenModuleAccountState = cbor::cbor_decode(module_state).unwrap();
    assert_eq!(state.allow_list, Some(true));
}
