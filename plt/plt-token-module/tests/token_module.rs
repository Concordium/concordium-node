use std::collections::HashMap;

use assert_matches::assert_matches;
use concordium_base::{
    common::cbor::{cbor_encode, value::Value},
    protocol_level_tokens::{TokenAmount, TokenModuleInitializationParameters},
};
use host_stub::{HostStub, TEST_ACCOUNT0, TEST_ACCOUNT1};
use plt_deployment_unit::token_module::{self, InitError};

mod host_stub;

/// In this example, the parameters are not a valid encoding.
#[test]
fn test_initialize_token_parameters_decode_failiure() {
    let mut host = HostStub::with_accounts([
        (0.into(), TEST_ACCOUNT0, None),
        (1.into(), TEST_ACCOUNT1, None),
    ]);
    let res = token_module::initialize_token(&mut host, vec![].into());
    assert_matches!(
        res,
        Err(InitError::DeserializationFailure(ref e))
            if e.to_string() == "IO error: failed to fill whole buffer"
    );
}

/// In this example, a parameter is missing from the required initialization parameters.
#[test]
fn test_initialize_token_parameters_missing() {
    let mut host = HostStub::with_accounts([
        (0.into(), TEST_ACCOUNT0, None),
        (1.into(), TEST_ACCOUNT1, None),
    ]);
    let parameters = TokenModuleInitializationParameters {
        name: None,
        metadata: Some("https://plt.token".to_owned().into()),
        governance_account: Some(TEST_ACCOUNT1.into()),
        allow_list: Some(true),
        deny_list: Some(false),
        initial_supply: None,
        mintable: Some(true),
        burnable: Some(true),
        additional: Default::default(),
    };
    let encoded_parameters = cbor_encode(&parameters).unwrap().into();
    let res = token_module::initialize_token(&mut host, encoded_parameters);
    assert_matches!(res,
        Err(InitError::DeserializationFailure(e))
            if e.to_string() == "Token name is missing"
    );
}

/// In this example, an unsupported additional parameter is present in the
/// initialization parameters.
#[test]
fn test_initiailize_token_additional_parameter() {
    let mut host = HostStub::with_accounts([
        (0.into(), TEST_ACCOUNT0, None),
        (1.into(), TEST_ACCOUNT1, None),
    ]);
    let mut additional = HashMap::with_capacity(1);
    additional.insert("_param1".into(), Value::Text("extravalue1".into()));
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some("https://plt.token".to_owned().into()),
        governance_account: Some(TEST_ACCOUNT1.into()),
        allow_list: Some(true),
        deny_list: Some(false),
        initial_supply: None,
        mintable: Some(true),
        burnable: Some(true),
        additional,
    };
    let encoded_parameters = cbor_encode(&parameters).unwrap().into();
    let res = token_module::initialize_token(&mut host, encoded_parameters);
    assert_matches!(
        res,
        Err(InitError::DeserializationFailure(e))
            if e.to_string() == "Unknown additional parameters: _param1"
    );
}

/// In this example, minimal parameters are specified to check defaulting
/// behaviour.
#[test]
fn test_initiailize_token_default_values() {
    let mut host = HostStub::with_accounts([
        (0.into(), TEST_ACCOUNT0, None),
        (1.into(), TEST_ACCOUNT1, None),
    ]);
    let init_accounts = host.accounts.clone();
    let metadata = "https://plt.token".to_owned().into();
    let encoded_metadata = cbor_encode(&metadata).unwrap();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata),
        governance_account: Some(TEST_ACCOUNT1.into()),
        allow_list: None,
        deny_list: None,
        initial_supply: None,
        mintable: None,
        burnable: None,
        additional: Default::default(),
    };
    let encoded_parameters = cbor_encode(&parameters).unwrap().into();
    token_module::initialize_token(&mut host, encoded_parameters).unwrap();
    assert_eq!(host.accounts, init_accounts);
    let mut expected_state = HashMap::with_capacity(3);
    expected_state.insert(b"\0\0name".into(), b"Protocol-level token".into());
    expected_state.insert(b"\0\0metadata".into(), encoded_metadata);
    expected_state.insert(b"\0\0governanceAccount".into(), 1u64.to_be_bytes().into());
    assert_eq!(host.state, expected_state);
}

/// In this example, the parameters are valid, no minting.
#[test]
fn test_initiailize_token_valid_1() {
    let mut host = HostStub::with_accounts([
        (0.into(), TEST_ACCOUNT0, None),
        (1.into(), TEST_ACCOUNT1, None),
    ]);
    let init_accounts = host.accounts.clone();
    let metadata = "https://plt.token".to_owned().into();
    let encoded_metadata = cbor_encode(&metadata).unwrap();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata),
        governance_account: Some(TEST_ACCOUNT1.into()),
        allow_list: Some(true),
        deny_list: Some(false),
        initial_supply: None,
        mintable: Some(true),
        burnable: Some(true),
        additional: Default::default(),
    };
    let encoded_parameters = cbor_encode(&parameters).unwrap().into();
    token_module::initialize_token(&mut host, encoded_parameters).unwrap();
    assert_eq!(host.accounts, init_accounts);
    let mut expected_state = HashMap::with_capacity(3);
    expected_state.insert(b"\0\0name".into(), b"Protocol-level token".into());
    expected_state.insert(b"\0\0metadata".into(), encoded_metadata);
    expected_state.insert(b"\0\0governanceAccount".into(), 1u64.to_be_bytes().into());
    expected_state.insert(b"\0\0allowList".into(), vec![]);
    expected_state.insert(b"\0\0mintable".into(), vec![]);
    expected_state.insert(b"\0\0burnable".into(), vec![]);
    assert_eq!(host.state, expected_state);
}

/// In this example, the parameters are valid, with minting.
#[test]
fn test_initiailize_token_valid_2() {
    let mut host = HostStub::with_accounts([
        (0.into(), TEST_ACCOUNT0, None),
        (1.into(), TEST_ACCOUNT1, None),
    ]);
    host.decimals = 2;
    let mut accounts = host.accounts.clone();
    let metadata = "https://plt.token".to_owned().into();
    let encoded_metadata = cbor_encode(&metadata).unwrap();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata),
        governance_account: Some(TEST_ACCOUNT1.into()),
        allow_list: Some(false),
        deny_list: Some(true),
        initial_supply: Some(TokenAmount::from_raw(500000, 2)),
        mintable: Some(false),
        burnable: Some(false),
        additional: Default::default(),
    };
    let encoded_parameters = cbor_encode(&parameters).unwrap().into();
    token_module::initialize_token(&mut host, encoded_parameters).unwrap();
    for account in accounts.iter_mut() {
        if account.index == 1.into() {
            account.balance = Some(500000);
        }
    }
    assert_eq!(host.accounts, accounts);
    let mut expected_state = HashMap::with_capacity(3);
    expected_state.insert(b"\0\0name".into(), b"Protocol-level token".into());
    expected_state.insert(b"\0\0metadata".into(), encoded_metadata);
    expected_state.insert(b"\0\0governanceAccount".into(), 1u64.to_be_bytes().into());
    expected_state.insert(b"\0\0denyList".into(), vec![]);
    assert_eq!(host.state, expected_state);
}

/// In this example, the parameters specify an initial supply with higher precision
/// than the token allows.
#[test]
fn test_initiailize_token_excessive_mint_decimals() {
    let mut host = HostStub::with_accounts([
        (0.into(), TEST_ACCOUNT0, None),
        (1.into(), TEST_ACCOUNT1, None),
    ]);
    host.decimals = 2;
    let metadata = "https://plt.token".to_owned().into();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata),
        governance_account: Some(TEST_ACCOUNT1.into()),
        allow_list: Some(false),
        deny_list: Some(false),
        initial_supply: Some(TokenAmount::from_raw(500000, 6)),
        mintable: Some(false),
        burnable: Some(false),
        additional: Default::default(),
    };
    let encoded_parameters = cbor_encode(&parameters).unwrap().into();
    let res = token_module::initialize_token(&mut host, encoded_parameters);
    assert_matches!(
        res,
        Err(InitError::InvalidMintAmount(e))
            if e.to_string() == "Token amount decimals mismatch: expected 2, found 6"
    );
}

/// In this example, the parameters specify an initial supply with less precision
/// than the token allows.
#[test]
fn test_initiailize_token_insufficient_mint_decimals() {
    let mut host = HostStub::with_accounts([
        (0.into(), TEST_ACCOUNT0, None),
        (1.into(), TEST_ACCOUNT1, None),
    ]);
    host.decimals = 6;
    let metadata = "https://plt.token".to_owned().into();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata),
        governance_account: Some(TEST_ACCOUNT1.into()),
        allow_list: Some(false),
        deny_list: Some(false),
        initial_supply: Some(TokenAmount::from_raw(500000, 2)),
        mintable: Some(false),
        burnable: Some(false),
        additional: Default::default(),
    };
    let encoded_parameters = cbor_encode(&parameters).unwrap().into();
    let res = token_module::initialize_token(&mut host, encoded_parameters);
    assert_matches!(
        res,
        Err(InitError::InvalidMintAmount(e))
            if e.to_string() == "Token amount decimals mismatch: expected 6, found 2"
    );
}
