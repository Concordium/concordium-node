//! Test of creating protocol-level token. Detailed tests should generally be implemented in
//! the tests of the token module in the `plt-token-module` crate. In the present file,
//! higher level tests are implemented.

use crate::block_state_stub::BlockStateStub;
use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, RawCbor, TokenAmount, TokenId,
    TokenModuleInitializationParameters, TokenModuleRef,
};
use concordium_base::updates::{CreatePlt, UpdatePayload};
use plt_scheduler::block_state_interface::BlockStateQuery;
use plt_scheduler::scheduler;
use plt_token_module::TOKEN_MODULE_REF;
use plt_types::types::events::BlockItemEvent;
use plt_types::types::execution::{ChainUpdateOutcome, FailureKind};
use plt_types::types::tokens::RawTokenAmount;

mod block_state_stub;

/// Test create protocol-level token.
#[test]
fn test_plt_create() {
    let mut stub = BlockStateStub::new();
    assert_eq!(stub.plt_update_instruction_sequence_number(), 0);

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
    let outcome =
        scheduler::execute_chain_update(&mut stub, payload).expect("create and initialize token");
    let events = assert_matches!(outcome, ChainUpdateOutcome::Success(events) => events);

    // Assert update instruction sequence number incremented
    assert_eq!(stub.plt_update_instruction_sequence_number(), 1);

    // Assert token module state
    let token = stub.token_by_id(&token_id).expect("created token");
    assert_eq!(stub.token_configuration(&token).token_id, token_id);
    assert_eq!(stub.token_configuration(&token).decimals, 4);
    assert_eq!(
        stub.token_configuration(&token).module_ref,
        TOKEN_MODULE_REF
    );

    // Assert circulating supply and governance account balance
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(0));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(0)
    );

    // Assert create token event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenCreated(create) => {
        assert_eq!(create.payload.token_id, token_id);
    });
}

/// Test create protocol-level token.
#[test]
fn test_plt_create_with_minting() {
    let mut stub = BlockStateStub::new();
    assert_eq!(stub.plt_update_instruction_sequence_number(), 0);

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
    let outcome =
        scheduler::execute_chain_update(&mut stub, payload).expect("create and initialize token");
    let events = assert_matches!(outcome, ChainUpdateOutcome::Success(events) => events);

    // Assert update instruction sequence number incremented
    assert_eq!(stub.plt_update_instruction_sequence_number(), 1);

    // Assert circulating supply and governance account balance
    let token = stub.token_by_id(&token_id).expect("created token");
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(5000)
    );

    // Assert create token and mint event
    assert_eq!(events.len(), 2);
    assert_matches!(&events[0], BlockItemEvent::TokenCreated(create) => {
        assert_eq!(create.payload.token_id, token_id);
    });
    assert_matches!(&events[1], BlockItemEvent::TokenMint(mint) => {
        assert_eq!(mint.token_id, token_id);
        assert_eq!(mint.amount.amount, RawTokenAmount(5000));
        assert_eq!(mint.amount.decimals, 4);
        assert_eq!(mint.target, stub.account_canonical_address(&gov_account));
    });
}

/// Test create protocol-level token where the token id is already used. Two token
/// ids which only differ in casing are considered equal.
#[test]
fn test_plt_create_duplicate_id() {
    let mut stub = BlockStateStub::new();

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
    let initialization_parameters: RawCbor = cbor::cbor_encode(&parameters).into();

    let token_id1: TokenId = "TestTokenId".parse().unwrap();
    let payload1 = UpdatePayload::CreatePlt(CreatePlt {
        token_id: token_id1.clone(),
        token_module: TOKEN_MODULE_REF,
        decimals: 4,
        initialization_parameters: initialization_parameters.clone(),
    });

    // Create first token
    scheduler::execute_chain_update(&mut stub, payload1).expect("create and initialize token");

    // Try to use same token id just with different casing
    let token_id2: TokenId = "testtokenid".parse().unwrap();
    let payload2 = UpdatePayload::CreatePlt(CreatePlt {
        token_id: token_id2.clone(),
        token_module: TOKEN_MODULE_REF,
        decimals: 4,
        initialization_parameters,
    });

    // Create second token
    let outcome = scheduler::execute_chain_update(&mut stub, payload2).unwrap();
    let failure_kind =
        assert_matches!(outcome, ChainUpdateOutcome::Failed(failure_kind) => failure_kind);

    assert_matches!(
        failure_kind,
        FailureKind::DuplicateTokenId(token_id) => {
            assert_eq!(token_id, token_id1);
        }
    );
}

/// Test create protocol-level token where the token module reference is to an unknown token module.
#[test]
fn test_plt_create_unknown_token_module_reference() {
    let mut stub = BlockStateStub::new();

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
    let initialization_parameters: RawCbor = cbor::cbor_encode(&parameters).into();

    let token_id: TokenId = "testtokenid".parse().unwrap();
    let unknown_module_ref = TokenModuleRef::new([0u8; 32]);
    let payload = UpdatePayload::CreatePlt(CreatePlt {
        token_id: token_id.clone(),
        token_module: unknown_module_ref,
        decimals: 4,
        initialization_parameters: initialization_parameters.clone(),
    });

    let outcome = scheduler::execute_chain_update(&mut stub, payload).unwrap();
    let failure_kind =
        assert_matches!(outcome, ChainUpdateOutcome::Failed(failure_kind) => failure_kind);

    assert_matches!(
        failure_kind,
        FailureKind::InvalidTokenModuleRef(module_ref) => {
            assert_eq!(module_ref, unknown_module_ref);
        }
    );
}

/// Test create protocol-level token where the token module returns an error.
#[test]
fn test_plt_create_token_module_initialization_error() {
    let mut stub = BlockStateStub::new();

    let gov_account = stub.create_account();
    let gov_holder_account = CborHolderAccount::from(stub.account_canonical_address(&gov_account));
    let metadata = MetadataUrl::from("https://plt.token".to_string());
    let parameters = TokenModuleInitializationParameters {
        // No name specified
        name: None,
        metadata: Some(metadata.clone()),
        governance_account: Some(gov_holder_account.clone()),
        allow_list: None,
        deny_list: None,
        initial_supply: None,
        mintable: None,
        burnable: None,
        additional: Default::default(),
    };
    let initialization_parameters: RawCbor = cbor::cbor_encode(&parameters).into();

    let token_id: TokenId = "testtokenid".parse().unwrap();
    let payload = UpdatePayload::CreatePlt(CreatePlt {
        token_id: token_id.clone(),
        token_module: TOKEN_MODULE_REF,
        decimals: 4,
        initialization_parameters: initialization_parameters.clone(),
    });

    let outcome = scheduler::execute_chain_update(&mut stub, payload).unwrap();
    let failure_kind =
        assert_matches!(outcome, ChainUpdateOutcome::Failed(failure_kind) => failure_kind);

    assert_matches!(
        failure_kind,
        FailureKind::TokenInitializeFailure(err) => {
            assert!(err.contains("Token name is missing"), "err: {}", err);
        }
    );
}
