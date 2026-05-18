//! Tests for token metadata update operations via the scheduler.

use crate::utils::TokenInitTestParams;
use crate::utils::entity_traits::scheduler::SchedulerOperations;
use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::{self, cbor};
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, OperationNotPermittedRejectReason, RawCbor, TokenAdminRole,
    TokenId, TokenModuleRejectReason, TokenModuleState, TokenOperation, TokenOperationsPayload,
    TokenUpdateAdminRolesDetails, UnsupportedOperationRejectReason,
};
use concordium_base::transactions::Payload;
use plt_block_state::entity::entity_test_stub;
use plt_scheduler_types::types::execution::TransactionOutcome;

use crate::utils::BlockStateLatest;

mod utils;

/// Succeeds in setting the token metadata.
#[test]
fn test_token_metadata_updates() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = utils::create_and_init_token(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        0,
        None,
    );

    // Check initial metadata via query.
    let token_info = block_state.query_token_info(&context, &token_id).unwrap();
    let initial_state: TokenModuleState =
        cbor::cbor_decode(&token_info.state.module_state).unwrap();
    assert_eq!(
        initial_state.metadata,
        Some(MetadataUrl::from("https://plt.token".to_string()))
    );

    let new_metadata_url = MetadataUrl {
        url: "https://plt2.token".to_string(),
        checksum_sha_256: Some([5u8; 32].into()),
        additional: Default::default(),
    };
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::UpdateMetadata(
            new_metadata_url.clone(),
        )])),
    };
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_account_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let token_info = block_state.query_token_info(&context, &token_id).unwrap();
    let module_state: TokenModuleState = cbor::cbor_decode(&token_info.state.module_state).unwrap();
    assert_eq!(module_state.metadata.unwrap(), new_metadata_url);
}

/// Succeeds for another account holding the updateMetadata role.
#[test]
fn test_new_account_with_role_succeeds_update_metadata() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = utils::create_and_init_token(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        2,
        None,
    );
    let account2 = context.external.create_account();

    // Assign the updateMetadata role to account2.
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
                roles: vec![TokenAdminRole::UpdateMetadata],
                account: CborHolderAccount::from(account2_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_account_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let new_metadata_url = MetadataUrl {
        url: "https://plt2.token".to_string(),
        checksum_sha_256: Some([5u8; 32].into()),
        additional: Default::default(),
    };
    let account2_addr = context
        .external
        .account_canonical_address(account2.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::UpdateMetadata(
            new_metadata_url.clone(),
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            account2.account_index(),
            account2_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let token_info = block_state.query_token_info(&context, &token_id).unwrap();
    let module_state: TokenModuleState = cbor::cbor_decode(&token_info.state.module_state).unwrap();
    assert_eq!(module_state.metadata.unwrap(), new_metadata_url);
}

/// Reject when governance account is not holding the updateMetadata role.
#[test]
fn test_role_authorization_update_metadata() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = utils::create_and_init_token(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        2,
        None,
    );

    // Remove updateMetadata role from governance account.
    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::UpdateMetadata],
                account: CborHolderAccount::from(gov_addr),
            },
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let new_metadata_url = MetadataUrl {
        url: "https://plt2.token".to_string(),
        checksum_sha_256: Some([5u8; 32].into()),
        additional: Default::default(),
    };
    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::UpdateMetadata(
            new_metadata_url,
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
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
            reason: Some(reason)
        }) => {
            assert_eq!(&reason, "sender is not authorized to perform the operation for this token");
            assert_eq!(address, CborHolderAccount::from(context.external.account_canonical_address(gov_account.account_index())));
        }
    );
}

/// Reject when additional metadata fields are provided.
#[test]
fn test_update_metadata_rejects_with_additional_data() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = utils::create_and_init_token(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        2,
        None,
    );

    let new_metadata_url = MetadataUrl {
        url: "https://plt2.token".to_string(),
        checksum_sha_256: Some([5u8; 32].into()),
        additional: [(
            "my_own_data_field".to_string(),
            common::cbor::value::Value::Text("custom_data".to_string()),
        )]
        .into(),
    };
    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::UpdateMetadata(
            new_metadata_url,
        )])),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
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
            reason: Some(reason),
            operation_type
        }) => {
            assert_eq!(&operation_type, "updateMetadata");
            assert_eq!(&reason, "Unknown additional metadata fields");
        }
    );
}
