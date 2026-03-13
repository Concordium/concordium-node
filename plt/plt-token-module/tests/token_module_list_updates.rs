use assert_matches::assert_matches;
use concordium_base::base::AccountIndex;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, OperationNotPermittedRejectReason, RawCbor, TokenAdminRole,
    TokenListUpdateDetails, TokenListUpdateEventDetails, TokenModuleAccountState,
    TokenModuleEventType, TokenModuleRejectReason, TokenOperation, TokenUpdateAdminRolesDetails,
    UnsupportedOperationRejectReason,
};
use plt_scheduler_interface::token_kernel_interface::TokenKernelQueries;
use plt_token_module::token_module;
use utils::kernel_stub::{KernelStub, TokenInitTestParams, TransactionExecutionTestImpl};

mod utils;

fn account_state_key(account_index: AccountIndex, subkey: &[u8]) -> Vec<u8> {
    let mut key = Vec::with_capacity(2 + 8 + subkey.len());
    key.extend_from_slice(&40307u16.to_le_bytes());
    key.extend_from_slice(&account_index.index.to_be_bytes());
    key.extend_from_slice(subkey);
    key
}

#[test]
fn test_allow_list_updates() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().allow_list());
    let target_account = stub.create_account();

    // Verify that the initial state is as expected
    let cbor =
        token_module::query_token_module_account_state(&stub, stub.account_index(&target_account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();
    assert_eq!(state.allow_list, Some(false));
    assert_eq!(state.deny_list, None);

    // Add an acccount to the allow list
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&target_account)),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    let cbor =
        token_module::query_token_module_account_state(&stub, stub.account_index(&target_account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();
    assert_eq!(state.allow_list, Some(true));
    assert_eq!(state.deny_list, None);

    // Remove the same acccount to the allow list
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::RemoveAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&target_account)),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    let cbor =
        token_module::query_token_module_account_state(&stub, stub.account_index(&target_account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();
    assert_eq!(state.allow_list, Some(false));
    assert_eq!(state.deny_list, None);

    // Verify that the expected events have been logged
    assert_eq!(stub.events().len(), 2);
    assert_eq!(
        stub.events()[0].0,
        TokenModuleEventType::AddAllowList.to_type_discriminator()
    );
    let add_event: TokenListUpdateEventDetails = cbor::cbor_decode(&stub.events()[0].1).unwrap();
    assert_eq!(
        add_event.target,
        CborHolderAccount::from(stub.account_address(&target_account))
    );
    assert_eq!(
        stub.events()[1].0,
        TokenModuleEventType::RemoveAllowList.to_type_discriminator()
    );
    let remove_event: TokenListUpdateEventDetails = cbor::cbor_decode(&stub.events()[1].1).unwrap();
    assert_eq!(
        remove_event.target,
        CborHolderAccount::from(stub.account_address(&target_account))
    );
}

#[test]
fn test_deny_list_updates() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().deny_list());
    let target_account = stub.create_account();

    // Verify that the initial state is as expected
    let cbor =
        token_module::query_token_module_account_state(&stub, stub.account_index(&target_account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();
    assert_eq!(state.allow_list, None);
    assert_eq!(state.deny_list, Some(false));

    // Add an acccount to the deny list
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&target_account)),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    let cbor =
        token_module::query_token_module_account_state(&stub, stub.account_index(&target_account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();
    assert_eq!(state.allow_list, None);
    assert_eq!(state.deny_list, Some(true));

    // Remove the same acccount to the deny list
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::RemoveDenyList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&target_account)),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    let cbor =
        token_module::query_token_module_account_state(&stub, stub.account_index(&target_account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();
    assert_eq!(state.allow_list, None);
    assert_eq!(state.deny_list, Some(false));

    // Verify that the expected events have been logged
    assert_eq!(stub.events().len(), 2);
    assert_eq!(
        stub.events()[0].0,
        TokenModuleEventType::AddDenyList.to_type_discriminator()
    );
    let add_event: TokenListUpdateEventDetails = cbor::cbor_decode(&stub.events()[0].1).unwrap();
    assert_eq!(
        add_event.target,
        CborHolderAccount::from(stub.account_address(&target_account))
    );
    assert_eq!(
        stub.events()[1].0,
        TokenModuleEventType::RemoveDenyList.to_type_discriminator()
    );
    let remove_event: TokenListUpdateEventDetails = cbor::cbor_decode(&stub.events()[1].1).unwrap();
    assert_eq!(
        remove_event.target,
        CborHolderAccount::from(stub.account_address(&target_account))
    );
}

#[test]
fn test_add_allow_list_reject_non_governance() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let _gov_account = stub.init_token(TokenInitTestParams::default().allow_list());
    let sender = stub.create_account();
    let target_account = stub.create_account();

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&target_account)),
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
            address: Some(address),
            reason: Some(reason),
            ..
        }) => {
            assert_eq!(address, CborHolderAccount::from(stub.account_address(&sender)));
            assert_eq!(reason, "sender is not authorized to perform the operation for this token");
        }
    );

    assert!(stub.events().is_empty());
    assert!(
        stub.lookup_token_state_value(account_state_key(
            stub.account_index(&target_account),
            b"allowList"
        ))
        .is_none()
    );
    let cbor =
        token_module::query_token_module_account_state(&stub, stub.account_index(&target_account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();
    assert_eq!(state.allow_list, Some(false));
    assert_eq!(state.deny_list, None);
    assert!(!stub.account_touched(&target_account));
}

#[test]
fn test_remove_allow_list_reject_non_governance() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let _gov_account = stub.init_token(TokenInitTestParams::default().allow_list());
    let sender = stub.create_account();
    let target_account = stub.create_account();

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let operations = vec![TokenOperation::RemoveAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&target_account)),
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
            address: Some(address),
            reason: Some(reason),
            ..
        }) => {
            assert_eq!(address, CborHolderAccount::from(stub.account_address(&sender)));
            assert_eq!(reason, "sender is not authorized to perform the operation for this token");
        }
    );

    assert!(stub.events().is_empty());
    assert!(
        stub.lookup_token_state_value(account_state_key(
            stub.account_index(&target_account),
            b"allowList"
        ))
        .is_none()
    );
    let cbor =
        token_module::query_token_module_account_state(&stub, stub.account_index(&target_account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();
    assert_eq!(state.allow_list, Some(false));
    assert_eq!(state.deny_list, None);
    assert!(!stub.account_touched(&target_account));
}

#[test]
fn test_add_deny_list_reject_non_governance() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let _gov_account = stub.init_token(TokenInitTestParams::default().deny_list());
    let sender = stub.create_account();
    let target_account = stub.create_account();

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let operations = vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&target_account)),
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
            address: Some(address),
            reason: Some(reason),
            ..
        }) => {
            assert_eq!(address, CborHolderAccount::from(stub.account_address(&sender)));
            assert_eq!(reason, "sender is not authorized to perform the operation for this token");
        }
    );

    assert!(stub.events().is_empty());
    assert!(
        stub.lookup_token_state_value(account_state_key(
            stub.account_index(&target_account),
            b"denyList"
        ))
        .is_none()
    );
    let cbor =
        token_module::query_token_module_account_state(&stub, stub.account_index(&target_account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();
    assert_eq!(state.allow_list, None);
    assert_eq!(state.deny_list, Some(false));
    assert!(!stub.account_touched(&target_account));
}

#[test]
fn test_remove_deny_list_reject_non_governance() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let _gov_account = stub.init_token(TokenInitTestParams::default().deny_list());
    let sender = stub.create_account();
    let target_account = stub.create_account();

    let mut execution = TransactionExecutionTestImpl::with_sender(sender);
    let operations = vec![TokenOperation::RemoveDenyList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&target_account)),
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
            address: Some(address),
            reason: Some(reason),
            ..
        }) => {
            assert_eq!(address, CborHolderAccount::from(stub.account_address(&sender)));
            assert_eq!(reason, "sender is not authorized to perform the operation for this token");
        }
    );

    assert!(stub.events().is_empty());
    assert!(
        stub.lookup_token_state_value(account_state_key(
            stub.account_index(&target_account),
            b"denyList"
        ))
        .is_none()
    );
    let cbor =
        token_module::query_token_module_account_state(&stub, stub.account_index(&target_account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();
    assert_eq!(state.allow_list, None);
    assert_eq!(state.deny_list, Some(false));
    assert!(!stub.account_touched(&target_account));
}

#[test]
fn test_add_allow_list_touches_account() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().allow_list());
    let target_account = stub.create_account();

    assert!(!stub.account_touched(&target_account));

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&target_account)),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    assert!(stub.account_touched(&target_account));
}

#[test]
fn test_remove_allow_list_touches_account() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().allow_list());
    let target_account = stub.create_account();

    assert!(!stub.account_touched(&target_account));

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::RemoveAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&target_account)),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    assert!(stub.account_touched(&target_account));
}

#[test]
fn test_add_deny_list_touches_account() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().deny_list());
    let target_account = stub.create_account();

    assert!(!stub.account_touched(&target_account));

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&target_account)),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    assert!(stub.account_touched(&target_account));
}

#[test]
fn test_remove_deny_list_touches_account() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().deny_list());
    let target_account = stub.create_account();

    assert!(!stub.account_touched(&target_account));

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::RemoveDenyList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&target_account)),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    assert!(stub.account_touched(&target_account));
}

#[test]
fn test_add_to_not_enabled_allow_list() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default());
    let allow_account = stub.create_account();

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&allow_account)),
    })];

    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);

    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::UnsupportedOperation(
            UnsupportedOperationRejectReason{
                index:0,
                operation_type,
                reason: Some(reason) })
            if reason == "feature not enabled" && operation_type == "addAllowList"
    );
}

#[test]
fn test_remove_from_not_enabled_allow_list() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default());
    let allow_account = stub.create_account();

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::RemoveAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&allow_account)),
    })];

    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);

    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::UnsupportedOperation(
            UnsupportedOperationRejectReason{
                index:0,
                operation_type,
                reason: Some(reason) })
            if reason == "feature not enabled" && operation_type == "removeAllowList"
    );
}

#[test]
fn test_add_to_not_enabled_deny_list() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default());
    let deny_account = stub.create_account();

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&deny_account)),
    })];

    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);

    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::UnsupportedOperation(
            UnsupportedOperationRejectReason{
                index:0,
                operation_type,
                reason: Some(reason) })
            if reason == "feature not enabled" && operation_type == "addDenyList"
    );
}

#[test]
fn test_remove_from_not_enabled_deny_list() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default());
    let deny_account = stub.create_account();

    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::RemoveDenyList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_address(&deny_account)),
    })];

    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );

    let reject_reason = utils::assert_reject_reason(&res);

    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::UnsupportedOperation(
            UnsupportedOperationRejectReason{
                index:0,
                operation_type,
                reason: Some(reason) })
            if reason == "feature not enabled" && operation_type == "removeDenyList"
    );
}

/// Reject addDenyList when governance account is not holding the updateDenylist role.
#[test]
fn test_reject_add_denylist_without_role() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().deny_list());

    // 1st transaction: removing updateDenylist role from governance account.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::RevokeAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::UpdateDenyList],
            account: CborHolderAccount::from(gov_account.1),
        },
    )];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );
    assert!(res.is_ok());

    // 2nd transaction: attempting to update deny list as governance account
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
        target: CborHolderAccount::from(gov_account.1),
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
            reason: Some(reason)
        }) => {
            assert_eq!(reason, "sender is not authorized to perform the operation for this token".to_string());
            assert_eq!(address, CborHolderAccount::from(gov_account.1));
        }
    );
}

/// Reject addAllowList when governance account is not holding the updateAllowlist role.
#[test]
fn test_reject_add_allowlist_without_role() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().allow_list());

    // 1st transaction: removing updateAllowlist role from governance account.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::RevokeAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::UpdateAllowList],
            account: CborHolderAccount::from(gov_account.1),
        },
    )];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );
    assert!(res.is_ok());

    // 2nd transaction: attempting to update allow list as governance account
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(gov_account.1),
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
            reason: Some(reason)
        }) => {
            assert_eq!(reason, "sender is not authorized to perform the operation for this token".to_string());
            assert_eq!(address, CborHolderAccount::from(gov_account.1));
        }
    );
}

/// Reject removeDenyList when governance account is not holding the updateDenylist role.
#[test]
fn test_reject_remove_denylist_without_role() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().deny_list());
    // 1st transaction: add governance account to deny list
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
        target: CborHolderAccount::from(gov_account.1),
    })];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );
    assert!(res.is_ok());

    // 2nd transaction: removing updateDenylist role from governance account.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::RevokeAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::UpdateDenyList],
            account: CborHolderAccount::from(gov_account.1),
        },
    )];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );
    assert!(res.is_ok());

    // 3nd transaction: attempting to update deny list as governance account
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::RemoveDenyList(TokenListUpdateDetails {
        target: CborHolderAccount::from(gov_account.1),
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
            reason: Some(reason)
        }) => {
            assert_eq!(reason, "sender is not authorized to perform the operation for this token".to_string());
            assert_eq!(address, CborHolderAccount::from(gov_account.1));
        }
    );
}

/// Reject removeAllowList when governance account is not holding the updateAllowlist role.
#[test]
fn test_reject_remove_allowlist_without_role() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().allow_list());
    // 1st transaction: add governance account to allow list
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(gov_account.1),
    })];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );
    assert!(res.is_ok());

    // 2nd transaction: removing updateAllowlist role from governance account.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::RevokeAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::UpdateAllowList],
            account: CborHolderAccount::from(gov_account.1),
        },
    )];
    let res = token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    );
    assert!(res.is_ok());

    // 3nd transaction: attempting to update allow list as governance account
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::RemoveAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(gov_account.1),
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
            reason: Some(reason)
        }) => {
            assert_eq!(reason, "sender is not authorized to perform the operation for this token".to_string());
            assert_eq!(address, CborHolderAccount::from(gov_account.1));
        }
    );
}

/// Succeeds for another account holding the updateDenylist role.
#[test]
fn test_succeeds_add_deny_list_new_account_with_role() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().deny_list());
    let account2 = stub.create_account();

    // 1st transaction: Assign the updateAllowlist role to an account.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::AssignAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::UpdateDenyList],
            account: CborHolderAccount::from(account2.1),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    // 2nd transaction: Add to deny list as account.
    let mut execution = TransactionExecutionTestImpl::with_sender(account2);
    let operations = vec![TokenOperation::AddDenyList(TokenListUpdateDetails {
        target: CborHolderAccount::from(gov_account.1),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    let cbor =
        token_module::query_token_module_account_state(&stub, stub.account_index(&gov_account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();
    assert_eq!(state.deny_list, Some(true));
}

/// Succeeds for another account holding the updateAllowlist role.
#[test]
fn test_succeeds_add_allow_list_new_account_with_role() {
    let mut stub = KernelStub::with_decimals(2, utils::LATEST_PROTOCOL_VERSION);
    let gov_account = stub.init_token(TokenInitTestParams::default().allow_list());
    let account2 = stub.create_account();

    // 1st transaction: Assign the updateAllowlist role to an account.
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![TokenOperation::AssignAdminRoles(
        TokenUpdateAdminRolesDetails {
            roles: vec![TokenAdminRole::UpdateAllowList],
            account: CborHolderAccount::from(account2.1),
        },
    )];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    // 2nd transaction: Add to allow list as account.
    let mut execution = TransactionExecutionTestImpl::with_sender(account2);
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(gov_account.1),
    })];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    let cbor =
        token_module::query_token_module_account_state(&stub, stub.account_index(&gov_account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();
    assert_eq!(state.allow_list, Some(true));
}
