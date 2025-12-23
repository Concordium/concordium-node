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
use plt_token_module::token_kernel_interface::{RawTokenAmount, TokenKernelQueries};
use plt_token_module::token_module::{
    self, TokenAmountDecimalsMismatchError, TokenInitializationError,
};

mod kernel_stub;

const TEST_ACCOUNT2: AccountAddress = AccountAddress([2u8; 32]);

/// In this example, the parameters are not a valid encoding.
#[test]
fn test_initialize_token_parameters_decode_failure() {
    let mut stub = KernelStub::new(0);
    let res = token_module::initialize_token(&mut stub, vec![].into());
    assert_matches!(
        &res,
        Err(TokenInitializationError::InvalidInitializationParameters(err))
            if err.contains("Error decoding token initialization parameters")
    );
}

/// In this example, a parameter is missing from the required initialization parameters.
#[test]
fn test_initialize_token_parameters_missing() {
    let mut stub = KernelStub::new(0);
    let gov_account = stub.create_account();
    let parameters = TokenModuleInitializationParameters {
        name: None,
        metadata: Some("https://plt.token".to_owned().into()),
        governance_account: Some(stub.account_canonical_address(&gov_account).into()),
        allow_list: Some(true),
        deny_list: Some(false),
        initial_supply: None,
        mintable: Some(true),
        burnable: Some(true),
        additional: Default::default(),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).unwrap().into();
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
    let mut stub = KernelStub::new(0);
    let gov_account = stub.create_account();
    let mut additional = HashMap::with_capacity(1);
    additional.insert("_param1".into(), Value::Text("extravalue1".into()));
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some("https://plt.token".to_owned().into()),
        governance_account: Some(stub.account_canonical_address(&gov_account).into()),
        allow_list: Some(true),
        deny_list: Some(false),
        initial_supply: None,
        mintable: Some(true),
        burnable: Some(true),
        additional,
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).unwrap().into();
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
    let mut stub = KernelStub::new(0);
    let gov_account = stub.create_account();
    let governance_holder_account =
        CborHolderAccount::from(stub.account_canonical_address(&gov_account));
    let metadata = MetadataUrl::from("https://plt.token".to_string());
    let encoded_metadata = cbor::cbor_encode(&metadata).unwrap();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata.clone()),
        governance_account: Some(governance_holder_account.clone()),
        allow_list: None,
        deny_list: None,
        initial_supply: None,
        mintable: None,
        burnable: None,
        additional: Default::default(),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).unwrap().into();
    token_module::initialize_token(&mut stub, encoded_parameters).unwrap();

    let mut expected_state = HashMap::with_capacity(3);
    expected_state.insert(b"\0\0name".into(), b"Protocol-level token".into());
    expected_state.insert(b"\0\0metadata".into(), encoded_metadata);
    expected_state.insert(
        b"\0\0governanceAccount".into(),
        stub.account_index(&gov_account).index.to_be_bytes().into(),
    );
    assert_eq!(stub.state, expected_state);

    let state: TokenModuleState =
        cbor::cbor_decode(token_module::query_token_module_state(&stub).unwrap()).unwrap();
    assert_eq!(state.name, Some("Protocol-level token".to_owned()));
    assert_eq!(state.metadata, Some(metadata));
    assert_eq!(state.governance_account, Some(governance_holder_account));
    assert_eq!(state.allow_list, Some(false));
    assert_eq!(state.deny_list, Some(false));
    assert_eq!(state.mintable, Some(false));
    assert_eq!(state.burnable, Some(false));
    assert_eq!(state.paused, Some(false));
    assert!(state.additional.is_empty());
}

// todo ar write rest of tests
// todo ar trim down plt model in base

/// In this example, the parameters are valid, no minting.
#[test]
fn test_initialize_token_no_minting() {
    let mut stub = KernelStub::new(0);
    let gov_account = stub.create_account();
    let metadata = MetadataUrl::from("https://plt.token".to_string());
    let encoded_metadata = cbor::cbor_encode(&metadata).unwrap();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata),
        governance_account: Some(stub.account_canonical_address(&gov_account).into()),
        allow_list: Some(true),
        deny_list: Some(false),
        initial_supply: None,
        mintable: Some(true),
        burnable: Some(true),
        additional: Default::default(),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).unwrap().into();
    token_module::initialize_token(&mut stub, encoded_parameters).unwrap();
    let mut expected_state = HashMap::with_capacity(3);
    expected_state.insert(b"\0\0name".into(), b"Protocol-level token".into());
    expected_state.insert(b"\0\0metadata".into(), encoded_metadata);
    expected_state.insert(
        b"\0\0governanceAccount".into(),
        stub.account_index(&gov_account).index.to_be_bytes().into(),
    );
    expected_state.insert(b"\0\0allowList".into(), vec![]);
    expected_state.insert(b"\0\0mintable".into(), vec![]);
    expected_state.insert(b"\0\0burnable".into(), vec![]);
    assert_eq!(stub.state, expected_state);
}

/// In this example, the parameters are valid, with minting.
#[test]
fn test_initialize_token_valid_2() {
    let mut stub = KernelStub::new(2);
    let gov_account = stub.create_account();
    let metadata = "https://plt.token".to_owned().into();
    let encoded_metadata = cbor::cbor_encode(&metadata).unwrap();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata),
        governance_account: Some(stub.account_canonical_address(&gov_account).into()),
        allow_list: Some(false),
        deny_list: Some(true),
        initial_supply: Some(TokenAmount::from_raw(500000, 2)),
        mintable: Some(false),
        burnable: Some(false),
        additional: Default::default(),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).unwrap().into();
    token_module::initialize_token(&mut stub, encoded_parameters).unwrap();
    assert_eq!(stub.account_balance(&gov_account), RawTokenAmount(500000));
    let mut expected_state = HashMap::with_capacity(3);
    expected_state.insert(b"\0\0name".into(), b"Protocol-level token".into());
    expected_state.insert(b"\0\0metadata".into(), encoded_metadata);
    expected_state.insert(
        b"\0\0governanceAccount".into(),
        stub.account_index(&gov_account).index.to_be_bytes().into(),
    );
    expected_state.insert(b"\0\0denyList".into(), vec![]);
    assert_eq!(stub.state, expected_state);
}

/// In this example, the parameters specify an initial supply with higher precision
/// than the token allows.
#[test]
fn test_initialize_token_excessive_mint_decimals() {
    let mut stub = KernelStub::new(2);
    let gov_account = stub.create_account();
    let metadata = "https://plt.token".to_owned().into();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata),
        governance_account: Some(stub.account_canonical_address(&gov_account).into()),
        allow_list: Some(false),
        deny_list: Some(false),
        initial_supply: Some(TokenAmount::from_raw(500000, 6)),
        mintable: Some(false),
        burnable: Some(false),
        additional: Default::default(),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).unwrap().into();
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
    let mut stub = KernelStub::new(6);
    let gov_account = stub.create_account();
    let metadata = "https://plt.token".to_owned().into();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata),
        governance_account: Some(stub.account_canonical_address(&gov_account).into()),
        allow_list: Some(false),
        deny_list: Some(false),
        initial_supply: Some(TokenAmount::from_raw(500000, 2)),
        mintable: Some(false),
        burnable: Some(false),
        additional: Default::default(),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).unwrap().into();
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
    let mut stub = KernelStub::new(0);
    let metadata = "https://plt.token".to_owned().into();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata),
        governance_account: Some(TEST_ACCOUNT2.into()),
        allow_list: Some(false),
        deny_list: Some(false),
        initial_supply: Some(TokenAmount::from_raw(500000, 2)),
        mintable: Some(false),
        burnable: Some(false),
        additional: Default::default(),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).unwrap().into();
    let res = token_module::initialize_token(&mut stub, encoded_parameters);
    assert_matches!(
        res,
        Err(TokenInitializationError::GovernanceAccountDoesNotExist(
            TEST_ACCOUNT2
        ))
    );
}
