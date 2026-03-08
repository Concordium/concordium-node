//! Test of creating protocol-level token. Detailed tests should generally be implemented in
//! the tests of the token module in the `plt-token-module` crate. In the present file,
//! higher level tests are implemented.

use assert_matches::assert_matches;
use block_state_external_stubbed::{BlockStateTestImpl, ExternalBlockStateStub};
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, RawCbor, TokenAmount, TokenId,
    TokenModuleInitializationParameters, TokenModuleRef,
};
use concordium_base::updates::{CreatePlt, UpdatePayload};
use plt_block_state::block_state::p10;
use plt_scheduler_types::types::events::BlockItemEvent;
use plt_scheduler_types::types::execution::{ChainUpdateOutcome, FailureKind};
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenHolder};
use plt_token_module::TOKEN_MODULE_REF;

mod block_state_external_stubbed;

/// Test create protocol-level token.
#[test]
fn test_plt_create() {
    test_plt_create_worker::<p10::PltBlockStateP10>()
}

fn test_plt_create_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    assert_eq!(external.plt_update_instruction_sequence_number, 0);

    let token_id: TokenId = "testtokenid".parse().unwrap();

    let gov_account = external.create_account();
    let gov_account_address = external.account_canonical_address(&gov_account);
    let gov_holder_account = CborHolderAccount::from(gov_account_address);
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
    };
    let initialization_parameters = cbor::cbor_encode(&parameters).into();

    let payload = UpdatePayload::CreatePlt(CreatePlt {
        token_id: token_id.clone(),
        token_module: TOKEN_MODULE_REF,
        decimals: 4,
        initialization_parameters,
    });
    let outcome = block_state
        .execute_chain_update(&mut external, payload)
        .expect("create and initialize token");
    let events = assert_matches!(outcome, ChainUpdateOutcome::Success(events) => events);

    // Assert update instruction sequence number incremented
    assert_eq!(external.plt_update_instruction_sequence_number, 1);

    // Assert token module state
    let token_info = block_state
        .query_token_info(&external, &token_id)
        .expect("created token");
    assert_eq!(token_info.token_id, token_id);
    assert_eq!(token_info.state.decimals, 4);
    assert_eq!(token_info.state.token_module_ref, TOKEN_MODULE_REF);

    // Assert circulating supply and governance account balance
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(0));
    assert_eq!(
        block_state.token_account_state(&external, &token_id, gov_account),
        None
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
    test_plt_create_with_minting_worker::<p10::PltBlockStateP10>();
}

fn test_plt_create_with_minting_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    assert_eq!(external.plt_update_instruction_sequence_number, 0);

    let token_id: TokenId = "testtokenid".parse().unwrap();

    let gov_account = external.create_account();
    let gov_holder_account =
        CborHolderAccount::from(external.account_canonical_address(&gov_account));
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
    };
    let initialization_parameters = cbor::cbor_encode(&parameters).into();

    let payload = UpdatePayload::CreatePlt(CreatePlt {
        token_id: token_id.clone(),
        token_module: TOKEN_MODULE_REF,
        decimals: 4,
        initialization_parameters,
    });
    let outcome = block_state
        .execute_chain_update(&mut external, payload)
        .expect("create and initialize token");
    let events = assert_matches!(outcome, ChainUpdateOutcome::Success(events) => events);

    // Assert update instruction sequence number incremented
    assert_eq!(external.plt_update_instruction_sequence_number, 1);

    // Assert circulating supply and governance account balance
    let token_info = block_state
        .query_token_info(&external, &token_id)
        .expect("created token");
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(5000));

    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, gov_account)
            .unwrap()
            .balance
            .amount,
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
        assert_eq!(mint.target, TokenHolder::Account(external.account_canonical_address(&gov_account)));
    });
}

/// Test create protocol-level token where the token id is already used. Two token
/// ids which only differ in casing are considered equal.
#[test]
fn test_plt_create_duplicate_id() {
    test_plt_create_duplicate_id_worker::<p10::PltBlockStateP10>();
}

fn test_plt_create_duplicate_id_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();

    let gov_account = external.create_account();
    let gov_holder_account =
        CborHolderAccount::from(external.account_canonical_address(&gov_account));
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
    block_state
        .execute_chain_update(&mut external, payload1)
        .expect("create and initialize token");

    // Try to use same token id just with different casing
    let token_id2: TokenId = "testtokenid".parse().unwrap();
    let payload2 = UpdatePayload::CreatePlt(CreatePlt {
        token_id: token_id2.clone(),
        token_module: TOKEN_MODULE_REF,
        decimals: 4,
        initialization_parameters,
    });

    // Create second token
    let outcome = block_state
        .execute_chain_update(&mut external, payload2)
        .unwrap();
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
    test_plt_create_unknown_token_module_reference_worker::<p10::PltBlockStateP10>();
}

fn test_plt_create_unknown_token_module_reference_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();

    let gov_account = external.create_account();
    let gov_holder_account =
        CborHolderAccount::from(external.account_canonical_address(&gov_account));
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

    let outcome = block_state
        .execute_chain_update(&mut external, payload)
        .unwrap();
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
    test_plt_create_token_module_initialization_error_worker::<p10::PltBlockStateP10>();
}

fn test_plt_create_token_module_initialization_error_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();

    let gov_account = external.create_account();
    let gov_holder_account =
        CborHolderAccount::from(external.account_canonical_address(&gov_account));
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
    };
    let initialization_parameters: RawCbor = cbor::cbor_encode(&parameters).into();

    let token_id: TokenId = "testtokenid".parse().unwrap();
    let payload = UpdatePayload::CreatePlt(CreatePlt {
        token_id: token_id.clone(),
        token_module: TOKEN_MODULE_REF,
        decimals: 4,
        initialization_parameters: initialization_parameters.clone(),
    });

    let outcome = block_state
        .execute_chain_update(&mut external, payload)
        .unwrap();
    let failure_kind =
        assert_matches!(outcome, ChainUpdateOutcome::Failed(failure_kind) => failure_kind);

    assert_matches!(
        failure_kind,
        FailureKind::TokenInitializeFailure(err) => {
            assert!(err.contains("Token name is missing"), "err: {}", err);
        }
    );
}
