use crate::kernel_stub::{KernelStub, TokenInitTestParams, TransactionExecutionTestImpl};
use assert_matches::assert_matches;
use concordium_base::base::AccountIndex;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, OperationNotPermittedRejectReason, RawCbor, TokenListUpdateDetails,
    TokenListUpdateEventDetails, TokenModuleAccountState, TokenModuleEventType,
    TokenModuleRejectReason, TokenOperation, UnsupportedOperationRejectReason,
};
use plt_scheduler_interface::token_kernel_interface::TokenKernelQueries;
use plt_token_module::token_module;

mod kernel_stub;
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
    let mut stub = KernelStub::with_decimals(0);
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
    assert_eq!(stub.events.len(), 2);
    assert_eq!(
        stub.events[0].0,
        TokenModuleEventType::AddAllowList.to_type_discriminator()
    );
    let add_event: TokenListUpdateEventDetails = cbor::cbor_decode(&stub.events[0].1).unwrap();
    assert_eq!(
        add_event.target,
        CborHolderAccount::from(stub.account_address(&target_account))
    );
    assert_eq!(
        stub.events[1].0,
        TokenModuleEventType::RemoveAllowList.to_type_discriminator()
    );
    let remove_event: TokenListUpdateEventDetails = cbor::cbor_decode(&stub.events[1].1).unwrap();
    assert_eq!(
        remove_event.target,
        CborHolderAccount::from(stub.account_address(&target_account))
    );
}

#[test]
fn test_deny_list_updates() {
    let mut stub = KernelStub::with_decimals(0);
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
    assert_eq!(stub.events.len(), 2);
    assert_eq!(
        stub.events[0].0,
        TokenModuleEventType::AddDenyList.to_type_discriminator()
    );
    let add_event: TokenListUpdateEventDetails = cbor::cbor_decode(&stub.events[0].1).unwrap();
    assert_eq!(
        add_event.target,
        CborHolderAccount::from(stub.account_address(&target_account))
    );
    assert_eq!(
        stub.events[1].0,
        TokenModuleEventType::RemoveDenyList.to_type_discriminator()
    );
    let remove_event: TokenListUpdateEventDetails = cbor::cbor_decode(&stub.events[1].1).unwrap();
    assert_eq!(
        remove_event.target,
        CborHolderAccount::from(stub.account_address(&target_account))
    );
}

#[test]
fn test_add_allow_list_reject_non_governance() {
    let mut stub = KernelStub::with_decimals(0);
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
            assert_eq!(reason, "sender is not the token governance account");
        }
    );

    assert!(stub.events.is_empty());
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
    let mut stub = KernelStub::with_decimals(0);
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
            assert_eq!(reason, "sender is not the token governance account");
        }
    );

    assert!(stub.events.is_empty());
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
    let mut stub = KernelStub::with_decimals(0);
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
            assert_eq!(reason, "sender is not the token governance account");
        }
    );

    assert!(stub.events.is_empty());
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
    let mut stub = KernelStub::with_decimals(0);
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
            assert_eq!(reason, "sender is not the token governance account");
        }
    );

    assert!(stub.events.is_empty());
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
    let mut stub = KernelStub::with_decimals(0);
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
    let mut stub = KernelStub::with_decimals(0);
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
    let mut stub = KernelStub::with_decimals(0);
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
    let mut stub = KernelStub::with_decimals(0);
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
    let mut stub = KernelStub::with_decimals(0);
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
    let mut stub = KernelStub::with_decimals(0);
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
    let mut stub = KernelStub::with_decimals(0);
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
    let mut stub = KernelStub::with_decimals(0);
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
