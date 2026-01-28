use std::collections::HashMap;

use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{CborHolderAccount, MetadataUrl, TokenModuleState};
use concordium_base::{
    common::cbor::value::Value,
    protocol_level_tokens::{TokenAmount, TokenModuleInitializationParameters},
};
use kernel_stub::KernelStub;
use plt_scheduler_interface::error::AccountNotFoundByAddressError;
use plt_scheduler_interface::token_kernel_interface::TokenKernelQueries;
use plt_token_module::token_module::{
    self, TokenAmountDecimalsMismatchError, TokenInitializationError,
};
use plt_types::types::tokens::RawTokenAmount;

mod kernel_stub;

const NON_EXISTING_ACCOUNT: AccountAddress = AccountAddress([2u8; 32]);

/// In this example, the parameters are not a valid encoding.
#[test]
fn test_initialize_token_parameters_decode_failure() {
    let mut stub = KernelStub::with_decimals(0);
    let res = token_module::initialize_token(&mut stub, vec![].into());
    assert_matches!(
        &res,
        Err(TokenInitializationError::CborSerialization(err)) => {
            let msg = err.to_string();
            assert!(msg.contains("IO error"), "msg: {}", msg);
        }
    );
}

/// In this example, a parameter is missing from the required initialization parameters.
#[test]
fn test_initialize_token_parameters_missing() {
    let mut stub = KernelStub::with_decimals(0);
    let gov_account = stub.create_account();
    let parameters = TokenModuleInitializationParameters {
        name: None,
        metadata: Some("https://plt.token".to_owned().into()),
        governance_account: Some(stub.account_address(&gov_account).into()),
        allow_list: Some(true),
        deny_list: Some(false),
        initial_supply: None,
        mintable: Some(true),
        burnable: Some(true),
        additional: Default::default(),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).into();
    let res = token_module::initialize_token(&mut stub, encoded_parameters);
    assert_matches!(res,
        Err(TokenInitializationError::InvalidInitializationParameters(err))
            if err == "Token name is missing"
    );
}

/// In this example, an unsupported additional parameter is present in the
/// initialization parameters.
#[test]
fn test_initialize_token_additional_parameter() {
    let mut stub = KernelStub::with_decimals(0);
    let gov_account = stub.create_account();
    let mut additional = HashMap::with_capacity(1);
    additional.insert("_param1".into(), Value::Text("extravalue1".into()));
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some("https://plt.token".to_owned().into()),
        governance_account: Some(stub.account_address(&gov_account).into()),
        allow_list: Some(true),
        deny_list: Some(false),
        initial_supply: None,
        mintable: Some(true),
        burnable: Some(true),
        additional,
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).into();
    let res = token_module::initialize_token(&mut stub, encoded_parameters);
    assert_matches!(
        res,
        Err(TokenInitializationError::InvalidInitializationParameters(err))
            if err == "Unknown additional parameters: _param1"
    );
}

/// In this example, minimal parameters are specified to check defaulting
/// behaviour.
#[test]
fn test_initialize_token_default_values() {
    let mut stub = KernelStub::with_decimals(0);
    let gov_account = stub.create_account();
    let gov_holder_account = CborHolderAccount::from(stub.account_address(&gov_account));
    let metadata = MetadataUrl::from("https://plt.token".to_string());
    let encoded_metadata = cbor::cbor_encode(&metadata);
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata.clone()),
        governance_account: Some(gov_holder_account.clone()),
        allow_list: None,
        deny_list: None,
        initial_supply: None,
        mintable: None,
        burnable: None,
        additional: Default::default(),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).into();
    token_module::initialize_token(&mut stub, encoded_parameters).unwrap();

    // Assertions directly on token state
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0name".into()),
        Some(b"Protocol-level token".into())
    );
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0metadata".into()),
        Some(encoded_metadata)
    );
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0governanceAccount".into()),
        Some(stub.account_index(&gov_account).index.to_be_bytes().into())
    );
    assert_eq!(stub.lookup_token_state_value(b"\0\0allowList".into()), None);
    assert_eq!(stub.lookup_token_state_value(b"\0\0denyList".into()), None);
    assert_eq!(stub.lookup_token_state_value(b"\0\0mintable".into()), None);
    assert_eq!(stub.lookup_token_state_value(b"\0\0burnable".into()), None);
    assert_eq!(stub.lookup_token_state_value(b"\0\0paused".into()), None);
    // assert governance account balance
    assert_eq!(stub.account_token_balance(&gov_account), RawTokenAmount(0));

    // Assertions using token module state query
    let state: TokenModuleState =
        cbor::cbor_decode(token_module::query_token_module_state(&stub).unwrap()).unwrap();
    assert_eq!(state.name, Some("Protocol-level token".to_owned()));
    assert_eq!(state.metadata, Some(metadata));
    assert_eq!(state.governance_account, Some(gov_holder_account));
    assert_eq!(state.allow_list, Some(false));
    assert_eq!(state.deny_list, Some(false));
    assert_eq!(state.mintable, Some(false));
    assert_eq!(state.burnable, Some(false));
    assert_eq!(state.paused, Some(false));
    assert!(state.additional.is_empty());
}

/// In this example, the parameters are valid, no minting.
#[test]
fn test_initialize_token_no_minting() {
    let mut stub = KernelStub::with_decimals(0);
    let gov_account = stub.create_account();
    let gov_holder_account = CborHolderAccount::from(stub.account_address(&gov_account));
    let metadata = MetadataUrl::from("https://plt.token".to_string());
    let encoded_metadata = cbor::cbor_encode(&metadata);
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata.clone()),
        governance_account: Some(gov_holder_account.clone()),
        allow_list: Some(true),
        deny_list: Some(false),
        initial_supply: None,
        mintable: Some(true),
        burnable: Some(true),
        additional: Default::default(),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).into();
    token_module::initialize_token(&mut stub, encoded_parameters).unwrap();

    // Assertions directly on token state
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0name".into()),
        Some(b"Protocol-level token".into())
    );
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0metadata".into()),
        Some(encoded_metadata)
    );
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0governanceAccount".into()),
        Some(stub.account_index(&gov_account).index.to_be_bytes().into())
    );
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0allowList".into()),
        Some(vec![])
    );
    assert_eq!(stub.lookup_token_state_value(b"\0\0denyList".into()), None);
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0mintable".into()),
        Some(vec![])
    );
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0burnable".into()),
        Some(vec![])
    );
    assert_eq!(stub.lookup_token_state_value(b"\0\0paused".into()), None);
    // assert governance account balance
    assert_eq!(stub.account_token_balance(&gov_account), RawTokenAmount(0));

    // Assertions using token module state query
    let state: TokenModuleState =
        cbor::cbor_decode(token_module::query_token_module_state(&stub).unwrap()).unwrap();
    assert_eq!(state.name, Some("Protocol-level token".to_owned()));
    assert_eq!(state.metadata, Some(metadata));
    assert_eq!(state.governance_account, Some(gov_holder_account));
    assert_eq!(state.allow_list, Some(true));
    assert_eq!(state.deny_list, Some(false));
    assert_eq!(state.mintable, Some(true));
    assert_eq!(state.burnable, Some(true));
    assert_eq!(state.paused, Some(false));
    assert!(state.additional.is_empty());
}

/// In this example, the parameters are valid, with minting.
#[test]
fn test_initialize_token_with_minting() {
    let mut stub = KernelStub::with_decimals(2);
    let gov_account = stub.create_account();
    let gov_holder_account = CborHolderAccount::from(stub.account_address(&gov_account));
    let metadata = MetadataUrl::from("https://plt.token".to_string());
    let encoded_metadata = cbor::cbor_encode(&metadata);
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata.clone()),
        governance_account: Some(gov_holder_account.clone()),
        allow_list: Some(false),
        deny_list: Some(true),
        initial_supply: Some(TokenAmount::from_raw(500000, 2)),
        mintable: Some(false),
        burnable: Some(false),
        additional: Default::default(),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).into();
    token_module::initialize_token(&mut stub, encoded_parameters).unwrap();

    // Assertions directly on token state
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0name".into()),
        Some(b"Protocol-level token".into())
    );
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0metadata".into()),
        Some(encoded_metadata)
    );
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0governanceAccount".into()),
        Some(stub.account_index(&gov_account).index.to_be_bytes().into())
    );
    assert_eq!(stub.lookup_token_state_value(b"\0\0allowList".into()), None);
    assert_eq!(
        stub.lookup_token_state_value(b"\0\0denyList".into()),
        Some(vec![])
    );
    assert_eq!(stub.lookup_token_state_value(b"\0\0mintable".into()), None);
    assert_eq!(stub.lookup_token_state_value(b"\0\0burnable".into()), None);
    assert_eq!(stub.lookup_token_state_value(b"\0\0paused".into()), None);
    // assert governance account balance
    assert_eq!(
        stub.account_token_balance(&gov_account),
        RawTokenAmount(500000)
    );

    // Assertions using token module state query
    let state: TokenModuleState =
        cbor::cbor_decode(token_module::query_token_module_state(&stub).unwrap()).unwrap();
    assert_eq!(state.name, Some("Protocol-level token".to_owned()));
    assert_eq!(state.metadata, Some(metadata));
    assert_eq!(state.governance_account, Some(gov_holder_account));
    assert_eq!(state.allow_list, Some(false));
    assert_eq!(state.deny_list, Some(true));
    assert_eq!(state.mintable, Some(false));
    assert_eq!(state.burnable, Some(false));
    assert_eq!(state.paused, Some(false));
    assert!(state.additional.is_empty());
}

/// In this example, the parameters specify an initial supply with higher precision
/// than the token allows.
#[test]
fn test_initialize_token_excessive_mint_decimals() {
    let mut stub = KernelStub::with_decimals(2);
    let gov_account = stub.create_account();
    let metadata = "https://plt.token".to_owned().into();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata),
        governance_account: Some(stub.account_address(&gov_account).into()),
        allow_list: Some(false),
        deny_list: Some(false),
        initial_supply: Some(TokenAmount::from_raw(500000, 6)),
        mintable: Some(false),
        burnable: Some(false),
        additional: Default::default(),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).into();
    let res = token_module::initialize_token(&mut stub, encoded_parameters);
    assert_matches!(
        res,
        Err(TokenInitializationError::MintAmountDecimalsMismatch(
            TokenAmountDecimalsMismatchError {
                expected: 2,
                found: 6
            }
        ))
    );
}

/// In this example, the parameters specify an initial supply with less precision
/// than the token allows.
#[test]
fn test_initialize_token_insufficient_mint_decimals() {
    let mut stub = KernelStub::with_decimals(6);
    let gov_account = stub.create_account();
    let metadata = "https://plt.token".to_owned().into();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata),
        governance_account: Some(stub.account_address(&gov_account).into()),
        allow_list: Some(false),
        deny_list: Some(false),
        initial_supply: Some(TokenAmount::from_raw(500000, 2)),
        mintable: Some(false),
        burnable: Some(false),
        additional: Default::default(),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).into();
    let res = token_module::initialize_token(&mut stub, encoded_parameters);
    assert_matches!(
        res,
        Err(TokenInitializationError::MintAmountDecimalsMismatch(
            TokenAmountDecimalsMismatchError {
                expected: 6,
                found: 2
            }
        ))
    );
}

/// In this example, the parameters specify an initial supply with less precision
/// than the token allows.
#[test]
fn test_initialize_token_non_existing_governance_account() {
    let mut stub = KernelStub::with_decimals(0);
    let metadata = "https://plt.token".to_owned().into();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata),
        governance_account: Some(NON_EXISTING_ACCOUNT.into()),
        allow_list: Some(false),
        deny_list: Some(false),
        initial_supply: Some(TokenAmount::from_raw(500000, 2)),
        mintable: Some(false),
        burnable: Some(false),
        additional: Default::default(),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).into();
    let res = token_module::initialize_token(&mut stub, encoded_parameters);
    assert_matches!(
        res,
        Err(TokenInitializationError::GovernanceAccountDoesNotExist(
            AccountNotFoundByAddressError(NON_EXISTING_ACCOUNT)
        ))
    );
}
