use crate::kernel_stub::{KernelStub, TokenInitTestParams, TransactionExecutionTestImpl};
use concordium_base::base::AccountIndex;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, RawCbor, TokenListUpdateDetails, TokenListUpdateEventDetails,
    TokenModuleAccountState, TokenModuleEventType, TokenOperation,
};
use plt_scheduler_interface::token_kernel_interface::TokenKernelQueries;
use plt_token_module::token_module;

mod kernel_stub;

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
fn test_list_updates_without_enabled_lists() {
    let mut stub = KernelStub::with_decimals(0);
    let gov_account = stub.init_token(TokenInitTestParams::default());
    let allow_account = stub.create_account();
    let deny_account = stub.create_account();

    // first we set the list state for the accounts
    let mut execution = TransactionExecutionTestImpl::with_sender(gov_account);
    let operations = vec![
        TokenOperation::AddAllowList(TokenListUpdateDetails {
            target: CborHolderAccount::from(stub.account_address(&allow_account)),
        }),
        TokenOperation::AddDenyList(TokenListUpdateDetails {
            target: CborHolderAccount::from(stub.account_address(&deny_account)),
        }),
    ];
    token_module::execute_token_update_transaction(
        &mut execution,
        &mut stub,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("executes successfully");

    // verify that the module state matches expectations
    assert_eq!(stub.lookup_token_state_value(b"\0\0allowList".into()), None);
    assert_eq!(stub.lookup_token_state_value(b"\0\0denyList".into()), None);

    // verify that the allow list has been set in the module state
    let allow_key = account_state_key(stub.account_index(&allow_account), b"allowList");
    assert_eq!(stub.lookup_token_state_value(allow_key), Some(vec![]));

    // and that it still shows as None in the account state
    let cbor =
        token_module::query_token_module_account_state(&stub, stub.account_index(&allow_account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();
    assert_eq!(state.allow_list, None);

    // verify that the deny list has been set in the module state
    let deny_key = account_state_key(stub.account_index(&deny_account), b"denyList");
    assert_eq!(stub.lookup_token_state_value(deny_key), Some(vec![]));

    // and that it still shows as None in the account state
    let cbor =
        token_module::query_token_module_account_state(&stub, stub.account_index(&deny_account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();
    assert_eq!(state.deny_list, None);

    // Finally, verify that the events are emitted
    assert_eq!(stub.events.len(), 2);
    assert_eq!(
        stub.events[0].0,
        TokenModuleEventType::AddAllowList.to_type_discriminator()
    );
    assert_eq!(
        stub.events[1].0,
        TokenModuleEventType::AddDenyList.to_type_discriminator()
    );
}
