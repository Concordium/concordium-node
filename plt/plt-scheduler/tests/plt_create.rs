//! Test of creating protocol-level token. Detailed tests should generally be implemented in
//! the tests of the token module in the `plt-token-module` crate. In the present file,
//! higher level tests are implemented.

use crate::block_state_stub::BlockStateStub;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, TokenAmount, TokenId, TokenModuleInitializationParameters,
};
use concordium_base::updates::{CreatePlt, UpdatePayload};
use plt_scheduler::block_state_interface::BlockStateQuery;
use plt_scheduler::{TOKEN_MODULE_REF, scheduler};
use plt_token_module::token_kernel_interface::RawTokenAmount;

mod block_state_stub;

/// Test create protocol-level token.
#[test]
fn test_plt_create() {
    let mut stub = BlockStateStub::new();
    assert_eq!(stub.plt_update_sequence_number(), 0);

    let token_id: TokenId = "testtokenid".parse().unwrap();

    let gov_account = stub.create_account();
    let gov_holder_account = CborHolderAccount::from(stub.account_canonical_address(&gov_account));
    let metadata = MetadataUrl::from("https://plt.token".to_string());
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
    let initialization_parameters = cbor::cbor_encode(&parameters).into();

    let payload = UpdatePayload::CreatePlt(CreatePlt {
        token_id: token_id.clone(),
        token_module: TOKEN_MODULE_REF,
        decimals: 4,
        initialization_parameters,
    });
    scheduler::execute_update_instruction(&mut stub, payload).expect("create and initialize token");

    assert_eq!(stub.plt_update_sequence_number(), 1);
    let token = stub.token_by_id(&token_id).expect("created token");
    assert_eq!(stub.token_configuration(&token).token_id, token_id);
    assert_eq!(stub.token_configuration(&token).decimals, 4);
    assert_eq!(
        stub.token_configuration(&token).module_ref,
        TOKEN_MODULE_REF
    );
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(0));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(0)
    );
}

/// Test create protocol-level token.
#[test]
#[ignore = "enable as part of https://linear.app/concordium/issue/PSR-29/implement-mint-and-burn"]
fn test_plt_create_with_minting() {
    let mut stub = BlockStateStub::new();
    assert_eq!(stub.plt_update_sequence_number(), 0);

    let token_id: TokenId = "testtokenid".parse().unwrap();

    let gov_account = stub.create_account();
    let gov_holder_account = CborHolderAccount::from(stub.account_canonical_address(&gov_account));
    let metadata = MetadataUrl::from("https://plt.token".to_string());
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata.clone()),
        governance_account: Some(gov_holder_account.clone()),
        allow_list: None,
        deny_list: None,
        initial_supply: Some(TokenAmount::from_raw(5000, 4)),
        mintable: None,
        burnable: None,
        additional: Default::default(),
    };
    let initialization_parameters = cbor::cbor_encode(&parameters).into();

    let payload = UpdatePayload::CreatePlt(CreatePlt {
        token_id: token_id.clone(),
        token_module: TOKEN_MODULE_REF,
        decimals: 4,
        initialization_parameters,
    });
    scheduler::execute_update_instruction(&mut stub, payload).expect("create and initialize token");

    assert_eq!(stub.plt_update_sequence_number(), 1);
    let token = stub.token_by_id(&token_id).expect("created token");
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(5000)
    );
}

/// Test create protocol-level token where the token id is already used. Two token
/// ids which only differ in casing are considered equal.
#[test]
fn test_plt_create_duplicate_id() {
    let mut stub = BlockStateStub::new();

    let token_id: TokenId = "testtokenid".parse().unwrap();

    let gov_account = stub.create_account();
    let gov_holder_account = CborHolderAccount::from(stub.account_canonical_address(&gov_account));
    let metadata = MetadataUrl::from("https://plt.token".to_string());
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
    let initialization_parameters = cbor::cbor_encode(&parameters).into();

    let payload = UpdatePayload::CreatePlt(CreatePlt {
        token_id: token_id.clone(),
        token_module: TOKEN_MODULE_REF,
        decimals: 4,
        initialization_parameters,
    });
    scheduler::execute_update_instruction(&mut stub, payload).expect("create and initialize token");

    let token = stub.token_by_id(&token_id).expect("created token");
    assert_eq!(stub.token_configuration(&token).token_id, token_id);
    assert_eq!(stub.token_configuration(&token).decimals, 4);
    assert_eq!(
        stub.token_configuration(&token).module_ref,
        TOKEN_MODULE_REF
    );
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(0));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(0)
    );
}
