use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, OperationNotPermittedRejectReason, RawCbor, TokenAdminRole,
    TokenModuleRejectReason, TokenOperation, TokenUpdateAdminRolesDetails,
};
use plt_scheduler_interface::token_kernel_interface::TokenKernelQueries;
use plt_token_module::token_module;
use utils::kernel_stub::{KernelStub, TokenInitTestParams, TransactionExecutionTestImpl};

mod utils;

/// Succeeds in setting the token metadata.
#[test]
fn test_token_metadata_updates() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default());

    let metadata = MetadataUrl::from("https://plt.token".to_string());
    let encoded_metadata = cbor::cbor_encode(&metadata);
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0metadata".into()),
        Some(encoded_metadata)
    );

    // Transaction
    let new_metadata_url = MetadataUrl {
        url: "https://plt2.token".to_string(),
        checksum_sha_256: Some([5u8; 32].into()),
        additional: Default::default(),
    };
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::UpdateMetadata(new_metadata_url.clone())];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    let new_encoded_metadata = cbor::cbor_encode(&new_metadata_url);
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0metadata".into()),
        Some(new_encoded_metadata)
    );
}

/// Succeeds for another account holding the updateMetadata role.
#[test]
fn test_new_account_with_role_succeeds() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default());
    let account2 = stub.create_account();

    // 1st transaction: Assign the updateMetadata role to an account.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::AssignAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::UpdateMetadata],
            account: CborHolderAccount::from(account2.1),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    // 2nd transaction: Pause as account.
    let new_metadata_url = MetadataUrl {
        url: "https://plt2.token".to_string(),
        checksum_sha_256: Some([5u8; 32].into()),
        additional: Default::default(),
    };
    let mut execution = TransactionExecutionTestImpl::with_sender(account2);
    let operations = vec![TokenOperation::UpdateMetadata(new_metadata_url.clone())];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    let new_encoded_metadata = cbor::cbor_encode(&new_metadata_url);
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0metadata".into()),
        Some(new_encoded_metadata)
    );
}

/// Reject when governance account is not holding the updateMetadata role.
#[test]
fn test_reject_without_role() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default());

    // 1st transaction: removing pause role from governance account.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::RevokeAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::UpdateMetadata],
            account: CborHolderAccount::from(gov_account.1),
        },
    )];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );
    assert!(res.is_ok());

    // 2nd transaction: attempting to pause as governance account
    let new_metadata_url = MetadataUrl {
        url: "https://plt2.token".to_string(),
        checksum_sha_256: Some([5u8; 32].into()),
        additional: Default::default(),
    };
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::UpdateMetadata(new_metadata_url)];
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
            reason: Some(reason)
        }) => {
            assert_eq!(reason, "sender is not authorized to perform the operation for this token".to_string());
            assert_eq!(address, CborHolderAccount::from(gov_account.1));
        }
    );
}

// TODO
// - Reject when additional metadata fields are provided.
