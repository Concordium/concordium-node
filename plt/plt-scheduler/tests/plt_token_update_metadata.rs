//! Tests for token metadata update operations via the scheduler.

use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::{self, cbor};
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, OperationNotPermittedRejectReason, RawCbor, TokenAdminRole,
    TokenId, TokenModuleRejectReason, TokenModuleState, TokenOperation, TokenOperationsPayload,
    TokenUpdateAdminRolesDetails, UnsupportedOperationRejectReason,
};
use concordium_base::transactions::Payload;
use plt_scheduler::{queries, scheduler};
use plt_scheduler_types::types::execution::TransactionOutcome;
use utils::block_state_external_stubbed::{
    BlockStateWithExternalStateStubbed, TokenInitTestParams,
};

mod utils;

/// Succeeds in setting the token metadata.
#[test]
fn test_token_metadata_updates() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 0, None);

    // Check initial metadata via query.
    let token_info = queries::query_token_info(stub.state(), &token_id).unwrap();
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
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        1.into(),
        0.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let token_info = queries::query_token_info(stub.state(), &token_id).unwrap();
    let module_state: TokenModuleState = cbor::cbor_decode(&token_info.state.module_state).unwrap();
    assert_eq!(module_state.metadata.unwrap(), new_metadata_url);
}

/// Succeeds for another account holding the updateMetadata role.
#[test]
fn test_new_account_with_role_succeeds_update_metadata() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 2, None);
    let account2 = stub.create_account();

    // Assign the updateMetadata role to account2.
    let account2_addr = CborHolderAccount::from(stub.account_canonical_address(&account2));
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AssignAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::UpdateMetadata],
                account: account2_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        1.into(),
        0.into(),
        stub.state_mut(),
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
    let account2_addr = stub.account_canonical_address(&account2);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::UpdateMetadata(
            new_metadata_url.clone(),
        )])),
    };
    let result = scheduler::execute_transaction(
        account2,
        account2_addr,
        1.into(),
        0.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let token_info = queries::query_token_info(stub.state(), &token_id).unwrap();
    let module_state: TokenModuleState = cbor::cbor_decode(&token_info.state.module_state).unwrap();
    assert_eq!(module_state.metadata.unwrap(), new_metadata_url);
}

/// Reject when governance account is not holding the updateMetadata role.
#[test]
fn test_role_authorization_update_metadata() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 2, None);

    // Remove updateMetadata role from governance account.
    let gov_addr = CborHolderAccount::from(stub.account_canonical_address(&gov_account));
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::UpdateMetadata],
                account: gov_addr,
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        1.into(),
        0.into(),
        stub.state_mut(),
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
    let gov_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::UpdateMetadata(
            new_metadata_url,
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        gov_addr,
        2.into(),
        0.into(),
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
            reason: Some(reason)
        }) => {
            assert_eq!(&reason, "sender is not authorized to perform the operation for this token");
            assert_eq!(address, CborHolderAccount::from(stub.account_canonical_address(&gov_account)));
        }
    );
}

/// Reject when additional metadata fields are provided.
#[test]
fn test_update_metadata_rejects_with_additional_data() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 2, None);

    let new_metadata_url = MetadataUrl {
        url: "https://plt2.token".to_string(),
        checksum_sha_256: Some([5u8; 32].into()),
        additional: [(
            "my_own_data_field".to_string(),
            common::cbor::value::Value::Text("custom_data".to_string()),
        )]
        .into(),
    };
    let gov_addr = stub.account_canonical_address(&gov_account);
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::UpdateMetadata(
            new_metadata_url,
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        gov_addr,
        1.into(),
        0.into(),
        stub.state_mut(),
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
